package io.iohk.ethereum.jsonrpc

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.Props
import akka.actor.Scheduler
import akka.util.ByteString
import akka.util.Timeout

import monix.eval.Task
import monix.execution

import scala.annotation.tailrec
import scala.util.Random

import io.iohk.ethereum.consensus.blocks.BlockGenerator
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.AkkaTaskOps.TaskActorOps
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.BloomFilter
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransaction
import io.iohk.ethereum.utils.FilterConfig
import io.iohk.ethereum.utils.TxPoolConfig

class FilterManager(
    blockchainReader: BlockchainReader,
    blockGenerator: BlockGenerator,
    keyStore: KeyStore,
    pendingTransactionsManager: ActorRef,
    filterConfig: FilterConfig,
    txPoolConfig: TxPoolConfig,
    externalSchedulerOpt: Option[Scheduler] = None
) extends Actor {

  import FilterManager._
  import akka.pattern.pipe
  import context.system

  def scheduler: Scheduler = externalSchedulerOpt.getOrElse(system.scheduler)
  implicit private val executionContext: execution.Scheduler = monix.execution.Scheduler(system.dispatcher)

  val maxBlockHashesChanges = 256

  var filters: Map[BigInt, Filter] = Map.empty

  var lastCheckBlocks: Map[BigInt, BigInt] = Map.empty

  var lastCheckTimestamps: Map[BigInt, Long] = Map.empty

  var filterTimeouts: Map[BigInt, Cancellable] = Map.empty

  implicit val timeout: Timeout = Timeout(txPoolConfig.pendingTxManagerQueryTimeout)

  override def receive: Receive = {
    case NewLogFilter(fromBlock, toBlock, address, topics) =>
      addFilterAndSendResponse(LogFilter(generateId(), fromBlock, toBlock, address, topics))
    case NewBlockFilter              => addFilterAndSendResponse(BlockFilter(generateId()))
    case NewPendingTransactionFilter => addFilterAndSendResponse(PendingTransactionFilter(generateId()))
    case UninstallFilter(id)         => uninstallFilter(id)
    case GetFilterLogs(id)           => getFilterLogs(id)
    case GetFilterChanges(id)        => getFilterChanges(id)
    case FilterTimeout(id)           => uninstallFilter(id)
    case gl: GetLogs =>
      val filter = LogFilter(0, gl.fromBlock, gl.toBlock, gl.address, gl.topics)
      sender() ! LogFilterLogs(getLogs(filter, None))
  }

  private def resetTimeout(id: BigInt): Unit = {
    filterTimeouts.get(id).foreach(_.cancel())
    val timeoutCancellable = scheduler.scheduleOnce(filterConfig.filterTimeout, self, FilterTimeout(id))
    filterTimeouts += (id -> timeoutCancellable)
  }

  private def addFilterAndSendResponse(filter: Filter): Unit = {
    filters += (filter.id -> filter)
    lastCheckBlocks += (filter.id -> blockchainReader.getBestBlockNumber())
    lastCheckTimestamps += (filter.id -> System.currentTimeMillis())
    resetTimeout(filter.id)
    sender() ! NewFilterResponse(filter.id)
  }

  private def uninstallFilter(id: BigInt): Unit = {
    filters -= id
    lastCheckBlocks -= id
    lastCheckTimestamps -= id
    filterTimeouts.get(id).foreach(_.cancel())
    filterTimeouts -= id
    sender() ! UninstallFilterResponse
  }

  private def getFilterLogs(id: BigInt): Unit = {
    val filterOpt = filters.get(id)
    filterOpt.foreach { _ =>
      lastCheckBlocks += (id -> blockchainReader.getBestBlockNumber())
      lastCheckTimestamps += (id -> System.currentTimeMillis())
    }
    resetTimeout(id)

    filterOpt match {
      case Some(logFilter: LogFilter) =>
        sender() ! LogFilterLogs(getLogs(logFilter))

      case Some(_: BlockFilter) =>
        sender() ! BlockFilterLogs(
          Nil
        ) // same as geth, returns empty array (otherwise it would have to return hashes of all blocks in the blockchain)

      case Some(_: PendingTransactionFilter) =>
        getPendingTransactions()
          .map { pendingTransactions =>
            PendingTransactionFilterLogs(pendingTransactions.map(_.stx.tx.hash))
          }
          .runToFuture
          .pipeTo(sender())

      case None =>
        sender() ! LogFilterLogs(Nil)
    }
  }

  private def getLogs(filter: LogFilter, startingBlockNumber: Option[BigInt] = None): Seq[TxLog] = {
    val bytesToCheckInBloomFilter = filter.address.map(a => Seq(a.bytes)).getOrElse(Nil) ++ filter.topics.flatten

    @tailrec
    def recur(currentBlockNumber: BigInt, toBlockNumber: BigInt, logsSoFar: Seq[TxLog]): Seq[TxLog] =
      if (currentBlockNumber > toBlockNumber) {
        logsSoFar
      } else {
        blockchainReader.getBlockHeaderByNumber(currentBlockNumber) match {
          case Some(header)
              if bytesToCheckInBloomFilter.isEmpty || BloomFilter.containsAnyOf(
                header.logsBloom,
                bytesToCheckInBloomFilter
              ) =>
            blockchainReader.getReceiptsByHash(header.hash) match {
              case Some(receipts) =>
                recur(
                  currentBlockNumber + 1,
                  toBlockNumber,
                  logsSoFar ++ getLogsFromBlock(
                    filter,
                    Block(header, blockchainReader.getBlockBodyByHash(header.hash).get),
                    receipts
                  )
                )
              case None => logsSoFar
            }
          case Some(_) => recur(currentBlockNumber + 1, toBlockNumber, logsSoFar)
          case None    => logsSoFar
        }
      }

    val bestBlockNumber = blockchainReader.getBestBlockNumber()

    val fromBlockNumber =
      startingBlockNumber.getOrElse(resolveBlockNumber(filter.fromBlock.getOrElse(BlockParam.Latest), bestBlockNumber))

    val toBlockNumber =
      resolveBlockNumber(filter.toBlock.getOrElse(BlockParam.Latest), bestBlockNumber)

    val logs = recur(fromBlockNumber, toBlockNumber, Nil)

    if (filter.toBlock.contains(BlockParam.Pending))
      logs ++ blockGenerator.getPendingBlock.map(p => getLogsFromBlock(filter, p.block, p.receipts)).getOrElse(Nil)
    else logs
  }

  private def getFilterChanges(id: BigInt): Unit = {
    val bestBlockNumber = blockchainReader.getBestBlockNumber()
    val lastCheckBlock = lastCheckBlocks.getOrElse(id, bestBlockNumber)
    val lastCheckTimestamp = lastCheckTimestamps.getOrElse(id, System.currentTimeMillis())

    val filterOpt = filters.get(id)
    filterOpt.foreach { _ =>
      lastCheckBlocks += (id -> bestBlockNumber)
      lastCheckTimestamps += (id -> System.currentTimeMillis())
    }
    resetTimeout(id)

    filterOpt match {
      case Some(logFilter: LogFilter) =>
        sender() ! LogFilterChanges(getLogs(logFilter, Some(lastCheckBlock + 1)))

      case Some(_: BlockFilter) =>
        sender() ! BlockFilterChanges(getBlockHashesAfter(lastCheckBlock).takeRight(maxBlockHashesChanges))

      case Some(_: PendingTransactionFilter) =>
        getPendingTransactions()
          .map { pendingTransactions =>
            val filtered = pendingTransactions.filter(_.addTimestamp > lastCheckTimestamp)
            PendingTransactionFilterChanges(filtered.map(_.stx.tx.hash))
          }
          .runToFuture
          .pipeTo(sender())

      case None =>
        sender() ! LogFilterChanges(Nil)
    }
  }

  private def getLogsFromBlock(filter: LogFilter, block: Block, receipts: Seq[Receipt]): Seq[TxLog] = {
    val bytesToCheckInBloomFilter = filter.address.map(a => Seq(a.bytes)).getOrElse(Nil) ++ filter.topics.flatten

    receipts.zipWithIndex.foldLeft(Nil: Seq[TxLog]) { case (logsSoFar, (receipt, txIndex)) =>
      if (
        bytesToCheckInBloomFilter.isEmpty || BloomFilter.containsAnyOf(
          receipt.logsBloomFilter,
          bytesToCheckInBloomFilter
        )
      ) {
        logsSoFar ++ receipt.logs.zipWithIndex
          .filter { case (log, _) =>
            filter.address.forall(_ == log.loggerAddress) && topicsMatch(log.logTopics, filter.topics)
          }
          .map { case (log, logIndex) =>
            val tx = block.body.transactionList(txIndex)
            TxLog(
              logIndex = logIndex,
              transactionIndex = txIndex,
              transactionHash = tx.hash,
              blockHash = block.header.hash,
              blockNumber = block.header.number,
              address = log.loggerAddress,
              data = log.data,
              topics = log.logTopics
            )
          }
      } else logsSoFar
    }
  }

  private def topicsMatch(logTopics: Seq[ByteString], filterTopics: Seq[Seq[ByteString]]): Boolean =
    logTopics.size >= filterTopics.size &&
      filterTopics.zip(logTopics).forall { case (filter, log) => filter.isEmpty || filter.contains(log) }

  private def getBlockHashesAfter(blockNumber: BigInt): Seq[ByteString] = {
    val bestBlock = blockchainReader.getBestBlockNumber()

    @tailrec
    def recur(currentBlockNumber: BigInt, hashesSoFar: Seq[ByteString]): Seq[ByteString] =
      if (currentBlockNumber > bestBlock) {
        hashesSoFar
      } else
        blockchainReader.getBlockHeaderByNumber(currentBlockNumber) match {
          case Some(header) => recur(currentBlockNumber + 1, hashesSoFar :+ header.hash)
          case None         => hashesSoFar
        }

    recur(blockNumber + 1, Nil)
  }

  private def getPendingTransactions(): Task[Seq[PendingTransaction]] =
    pendingTransactionsManager
      .askFor[PendingTransactionsManager.PendingTransactionsResponse](PendingTransactionsManager.GetPendingTransactions)
      .flatMap { response =>
        keyStore.listAccounts() match {
          case Right(accounts) =>
            Task.now(
              response.pendingTransactions.filter(pt => accounts.contains(pt.stx.senderAddress))
            )
          case Left(_) => Task.raiseError(new RuntimeException("Cannot get account list"))
        }
      }

  private def generateId(): BigInt = BigInt(Random.nextLong()).abs

  private def resolveBlockNumber(blockParam: BlockParam, bestBlockNumber: BigInt): BigInt =
    blockParam match {
      case BlockParam.WithNumber(blockNumber) => blockNumber
      case BlockParam.Earliest                => 0
      case BlockParam.Latest                  => bestBlockNumber
      case BlockParam.Pending                 => bestBlockNumber
    }
}

object FilterManager {
  def props(
      blockchainReader: BlockchainReader,
      blockGenerator: BlockGenerator,
      keyStore: KeyStore,
      pendingTransactionsManager: ActorRef,
      filterConfig: FilterConfig,
      txPoolConfig: TxPoolConfig
  ): Props =
    Props(
      new FilterManager(
        blockchainReader,
        blockGenerator,
        keyStore,
        pendingTransactionsManager,
        filterConfig,
        txPoolConfig
      )
    )

  sealed trait Filter {
    def id: BigInt
  }
  case class LogFilter(
      override val id: BigInt,
      fromBlock: Option[BlockParam],
      toBlock: Option[BlockParam],
      address: Option[Address],
      topics: Seq[Seq[ByteString]]
  ) extends Filter
  case class BlockFilter(override val id: BigInt) extends Filter
  case class PendingTransactionFilter(override val id: BigInt) extends Filter

  case class NewLogFilter(
      fromBlock: Option[BlockParam],
      toBlock: Option[BlockParam],
      address: Option[Address],
      topics: Seq[Seq[ByteString]]
  )
  case object NewBlockFilter
  case object NewPendingTransactionFilter
  case class NewFilterResponse(id: BigInt)

  case class UninstallFilter(id: BigInt)
  case object UninstallFilterResponse

  case class GetFilterLogs(id: BigInt)
  case class GetFilterChanges(id: BigInt)

  case class GetLogs(
      fromBlock: Option[BlockParam],
      toBlock: Option[BlockParam],
      address: Option[Address],
      topics: Seq[Seq[ByteString]]
  )

  case class TxLog(
      logIndex: BigInt,
      transactionIndex: BigInt,
      transactionHash: ByteString,
      blockHash: ByteString,
      blockNumber: BigInt,
      address: Address,
      data: ByteString,
      topics: Seq[ByteString]
  )

  sealed trait FilterChanges
  case class LogFilterChanges(logs: Seq[TxLog]) extends FilterChanges
  case class BlockFilterChanges(blockHashes: Seq[ByteString]) extends FilterChanges
  case class PendingTransactionFilterChanges(txHashes: Seq[ByteString]) extends FilterChanges

  sealed trait FilterLogs
  case class LogFilterLogs(logs: Seq[TxLog]) extends FilterLogs
  case class BlockFilterLogs(blockHashes: Seq[ByteString]) extends FilterLogs
  case class PendingTransactionFilterLogs(txHashes: Seq[ByteString]) extends FilterLogs

  private case class FilterTimeout(id: BigInt)
}
