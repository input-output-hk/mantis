package io.iohk.ethereum.jsonrpc

import akka.actor.{Actor, ActorRef, Cancellable, Props, Scheduler}
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.BloomFilter
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils.FilterConfig

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

class FilterManager(
    blockchain: Blockchain,
    appStateStorage: AppStateStorage,
    keyStore: KeyStore,
    pendingTransactionsManager: ActorRef,
    config: FilterConfig,
    externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor {

  import FilterManager._
  import akka.pattern.{ask, pipe}
  import context.system

  def scheduler: Scheduler = externalSchedulerOpt getOrElse system.scheduler

  val maxBlockHashesChanges = 256

  var filters: Map[BigInt, Filter] = Map.empty

  var lastCheckBlocks: Map[BigInt, BigInt] = Map.empty

  var filterTimeouts: Map[BigInt, Cancellable] = Map.empty

  implicit val timeout = Timeout(5.seconds)

  override def receive: Receive = {
    case NewLogFilter(fromBlock, toBlock, address, topics) => addFilterAndSendResponse(LogFilter(generateId(), fromBlock, toBlock, address, topics))
    case NewBlockFilter() => addFilterAndSendResponse(BlockFilter(generateId()))
    case NewPendingTransactionFilter() => addFilterAndSendResponse(PendingTransactionFilter(generateId()))
    case UninstallFilter(id) => uninstallFilter(id)
    case GetFilterLogs(id) => getFilterLogs(id)
    case GetFilterChanges(id) => getFilterChanges(id)
    case FilterTimeout(id) => uninstallFilter(id)
    case gl: GetLogs =>
      val filter = LogFilter(0, gl.fromBlock, gl.toBlock, gl.address, gl.topics)
      sender() ! LogFilterLogs(getLogs(filter, None))
  }

  private def resetTimeout(id: BigInt): Unit = {
    filterTimeouts.get(id).foreach(_.cancel())
    val timeoutCancellable = scheduler.scheduleOnce(config.filterTimeout, self, FilterTimeout(id))
    filterTimeouts += (id -> timeoutCancellable)
  }

  private def addFilterAndSendResponse(filter: Filter): Unit = {
    filters += (filter.id -> filter)
    lastCheckBlocks += (filter.id -> appStateStorage.getBestBlockNumber())
    resetTimeout(filter.id)
    sender() ! NewFilterResponse(filter.id)
  }

  private def uninstallFilter(id: BigInt): Unit = {
    filters -= id
    lastCheckBlocks -= id
    filterTimeouts.get(id).foreach(_.cancel())
    filterTimeouts -= id
    sender() ! UninstallFilterResponse()
  }

  private def getFilterLogs(id: BigInt): Unit = {
    val filterOpt = filters.get(id)
    filterOpt.foreach { _ => lastCheckBlocks += (id -> appStateStorage.getBestBlockNumber()) }
    resetTimeout(id)

    filterOpt match {
      case Some(logFilter: LogFilter) =>
        sender() ! LogFilterLogs(getLogs(logFilter))

      case Some(_: BlockFilter) =>
        sender() ! BlockFilterLogs(Nil) // same as geth, returns empty array (otherwise it would have to return hashes of all blocks in the blockchain)

      case Some(_: PendingTransactionFilter) =>
        getPendingTransactions().map { pendingTransactions =>
          PendingTransactionFilterLogs(pendingTransactions.map(_.hash))
        }.pipeTo(sender())

      case None =>
        sender() ! LogFilterLogs(Nil)
    }
  }

  private def getLogs(filter: LogFilter, startingBlockNumber: Option[BigInt] = None): Seq[Log] = {
    val bytesToCheckInBloomFilter = filter.address.map(a => Seq(a.bytes)).getOrElse(Nil) ++ filter.topics.flatten

    @tailrec
    def recur(currentBlockNumber: BigInt, toBlockNumber: BigInt, logsSoFar: Seq[Log]): Seq[Log] = {
      if (currentBlockNumber > toBlockNumber) {
        logsSoFar
      } else {
        blockchain.getBlockHeaderByNumber(currentBlockNumber) match {
          case Some(blockHeader) if bytesToCheckInBloomFilter.isEmpty || BloomFilter.containsAnyOf(blockHeader.logsBloom, bytesToCheckInBloomFilter) =>
            blockchain.getReceiptsByHash(blockHeader.hash) match {
              case Some(receipts) => recur(currentBlockNumber + 1, toBlockNumber, logsSoFar ++ getLogsFromBlock(filter, blockHeader, receipts))
              case None => logsSoFar
            }
          case Some(_) => recur(currentBlockNumber + 1, toBlockNumber, logsSoFar)
          case None => logsSoFar
        }
      }
    }

    val fromBlockNumber =
      startingBlockNumber.getOrElse(resolveBlockNumber(filter.fromBlock.getOrElse(BlockParam.Latest), appStateStorage))

    val toBlockNumber =
      resolveBlockNumber(filter.toBlock.getOrElse(BlockParam.Latest), appStateStorage)

    recur(fromBlockNumber, toBlockNumber, Nil)
  }

  private def getFilterChanges(id: BigInt): Unit = {
    val bestBlockNumber = appStateStorage.getBestBlockNumber()
    val lastCheckBlock = lastCheckBlocks.getOrElse(id, bestBlockNumber)

    val filterOpt = filters.get(id)
    filterOpt.foreach { _ => lastCheckBlocks += (id -> bestBlockNumber) }
    resetTimeout(id)

    filterOpt match {
      case Some(logFilter: LogFilter) =>
        sender() ! LogFilterChanges(getLogs(logFilter, Some(lastCheckBlock + 1)))

      case Some(_: BlockFilter) =>
        sender() ! BlockFilterChanges(getBlockHashesAfter(lastCheckBlock).takeRight(maxBlockHashesChanges))

      case Some(_: PendingTransactionFilter) =>
        getPendingTransactions().map { pendingTransactions => // TODO: should this return only pending transactions added since last poll?
          PendingTransactionFilterChanges(pendingTransactions.map(_.hash))
        }.pipeTo(sender())

      case None =>
        sender() ! LogFilterChanges(Nil)
    }
  }

  private def getLogsFromBlock(filter: LogFilter, blockHeader: BlockHeader, receipts: Seq[Receipt]): Seq[Log] = {
    val bytesToCheckInBloomFilter = filter.address.map(a => Seq(a.bytes)).getOrElse(Nil) ++ filter.topics.flatten

    receipts.zipWithIndex.foldLeft(Seq[Log]()) { case (logsSoFar, (receipt, txIndex)) =>
      if (bytesToCheckInBloomFilter.isEmpty || BloomFilter.containsAnyOf(receipt.logsBloomFilter, bytesToCheckInBloomFilter)) {
        logsSoFar ++ receipt.logs.zipWithIndex
        .filter { case (log, _) => filter.address.forall(_ == log.loggerAddress) && topicsMatch(log.logTopics, filter.topics) }
        .map { case (log, logIndex) =>
          val blockBody = blockchain.getBlockBodyByHash(blockHeader.hash).get
          val tx = blockBody.transactionList(txIndex)
          Log(
            logIndex = logIndex,
            transactionIndex = Some(txIndex), // TODO: None is this is a pending block (?)
            transactionHash = Some(tx.hash), // TODO: None is this is a pending block (?)
            blockHash = blockHeader.hash,
            blockNumber = blockHeader.number,
            address = log.loggerAddress,
            data = log.data,
            topics = log.logTopics)
        }
      } else logsSoFar
    }
  }

  private def topicsMatch(logTopics: Seq[ByteString], filterTopics: Seq[Seq[ByteString]]): Boolean = {
    (filterTopics zip logTopics).forall { case (filter, log) => filter.isEmpty || filter.contains(log) }
  }

  private def getBlockHashesAfter(blockNumber: BigInt): Seq[ByteString] = {
    val bestBlock = appStateStorage.getBestBlockNumber()

    @tailrec
    def recur(currentBlockNumber: BigInt, hashesSoFar: Seq[ByteString]): Seq[ByteString] = {
      if (currentBlockNumber > bestBlock) {
        hashesSoFar
      } else blockchain.getBlockHeaderByNumber(currentBlockNumber) match {
        case Some(header) => recur(currentBlockNumber + 1, hashesSoFar :+ header.hash)
        case None => hashesSoFar
      }
    }

    recur(blockNumber + 1, Nil)
  }

  private def getPendingTransactions(): Future[Seq[SignedTransaction]] = {
    (pendingTransactionsManager ? PendingTransactionsManager.GetPendingTransactions)
      .mapTo[PendingTransactionsManager.PendingTransactions]
      .flatMap { case PendingTransactionsManager.PendingTransactions(pendingTransactions) =>
        keyStore.listAccounts() match {
          case Right(accounts) => Future.successful(pendingTransactions.filter(pt => accounts.contains(pt.senderAddress)))
          case Left(_) => Future.failed(new RuntimeException("Cannot get account list"))
        }
      }
  }

  private def generateId(): BigInt = Math.abs(Random.nextLong())

  private def resolveBlockNumber(blockParam: BlockParam, appStateStorage: AppStateStorage): BigInt = {
    blockParam match {
      case BlockParam.WithNumber(blockNumber) => blockNumber
      case BlockParam.Earliest => 0
      case BlockParam.Latest => appStateStorage.getBestBlockNumber()
      case BlockParam.Pending => appStateStorage.getBestBlockNumber() + 1
    }
  }
}

object FilterManager {
  def props(blockchain: Blockchain,
            appStateStorage: AppStateStorage,
            keyStore: KeyStore,
            pendingTransactionsManager: ActorRef,
            config: FilterConfig): Props =
    Props(new FilterManager(blockchain, appStateStorage, keyStore, pendingTransactionsManager, config))

  sealed trait Filter {
    def id: BigInt
  }
  case class LogFilter(
      override val id: BigInt,
      fromBlock: Option[BlockParam],
      toBlock: Option[BlockParam],
      address: Option[Address],
      topics: Seq[Seq[ByteString]]) extends Filter
  case class BlockFilter(override val id: BigInt) extends Filter
  case class PendingTransactionFilter(override val id: BigInt) extends Filter

  case class NewLogFilter(fromBlock: Option[BlockParam], toBlock: Option[BlockParam], address: Option[Address], topics: Seq[Seq[ByteString]])
  case class NewBlockFilter()
  case class NewPendingTransactionFilter()
  case class NewFilterResponse(id: BigInt)

  case class UninstallFilter(id: BigInt)
  case class UninstallFilterResponse()

  case class GetFilterLogs(id: BigInt)
  case class GetFilterChanges(id: BigInt)

  case class GetLogs(fromBlock: Option[BlockParam], toBlock: Option[BlockParam], address: Option[Address], topics: Seq[Seq[ByteString]])

  case class Log(
      logIndex: BigInt,
      transactionIndex: Option[BigInt],
      transactionHash: Option[ByteString],
      blockHash: ByteString,
      blockNumber: BigInt,
      address: Address,
      data: ByteString,
      topics: Seq[ByteString])

  sealed trait FilterChanges
  case class LogFilterChanges(logs: Seq[Log]) extends FilterChanges
  case class BlockFilterChanges(blockHashes: Seq[ByteString]) extends FilterChanges
  case class PendingTransactionFilterChanges(txHashes: Seq[ByteString]) extends FilterChanges

  sealed trait FilterLogs
  case class LogFilterLogs(logs: Seq[Log]) extends FilterLogs
  case class BlockFilterLogs(blockHashes: Seq[ByteString]) extends FilterLogs
  case class PendingTransactionFilterLogs(txHashes: Seq[ByteString]) extends FilterLogs
  // TODO: getTransactionByHash method should also query pending transactions

  private case class FilterTimeout(id: BigInt)
}
