package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.{ByteString, Timeout}
import cats.syntax.either._
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.consensus.blocks.BlockGenerator
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.domain.{BlockHeader, SignedTransaction, _}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import io.iohk.ethereum.jsonrpc.FilterManager.{FilterChanges, FilterLogs, LogFilterLogs}
import io.iohk.ethereum.jsonrpc.{FilterManager => FM}
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, Ledger, StxLedger}
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.{PendingTransaction, PendingTransactionsResponse}
import io.iohk.ethereum.utils._
import monix.eval.Task

import java.util.Date
import java.util.concurrent.atomic.AtomicReference
import scala.collection.concurrent.{TrieMap, Map => ConcurrentMap}
import scala.concurrent.duration.FiniteDuration
import scala.language.existentials
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

// scalastyle:off number.of.methods number.of.types file.size.limit
object EthService {

  case class ProtocolVersionRequest()
  case class ProtocolVersionResponse(value: String)

  case class GetTransactionByBlockHashAndIndexRequest(blockHash: ByteString, transactionIndex: BigInt)
  case class GetTransactionByBlockHashAndIndexResponse(transactionResponse: Option[TransactionResponse])

  case class GetTransactionByHashRequest(txHash: ByteString)
  case class GetTransactionByHashResponse(txResponse: Option[TransactionResponse])

  case class GetTransactionReceiptRequest(txHash: ByteString)
  case class GetTransactionReceiptResponse(txResponse: Option[TransactionReceiptResponse])

  case class GetTransactionByBlockNumberAndIndexRequest(block: BlockParam, transactionIndex: BigInt)
  case class GetTransactionByBlockNumberAndIndexResponse(transactionResponse: Option[TransactionResponse])

  case class RawTransactionResponse(transactionResponse: Option[SignedTransaction])

  case class GetGasPriceRequest()
  case class GetGasPriceResponse(price: BigInt)

  case class SyncingRequest()
  case class SyncingStatus(
      startingBlock: BigInt,
      currentBlock: BigInt,
      highestBlock: BigInt,
      knownStates: BigInt,
      pulledStates: BigInt
  )
  case class SyncingResponse(syncStatus: Option[SyncingStatus])

  case class SendRawTransactionRequest(data: ByteString)
  case class SendRawTransactionResponse(transactionHash: ByteString)

  sealed trait BlockParam

  object BlockParam {
    case class WithNumber(n: BigInt) extends BlockParam
    case object Latest extends BlockParam
    case object Pending extends BlockParam
    case object Earliest extends BlockParam
  }

  case class CallTx(
      from: Option[ByteString],
      to: Option[ByteString],
      gas: Option[BigInt],
      gasPrice: BigInt,
      value: BigInt,
      data: ByteString
  )

  case class IeleCallTx(
      from: Option[ByteString],
      to: Option[ByteString],
      gas: Option[BigInt],
      gasPrice: BigInt,
      value: BigInt,
      function: Option[String] = None,
      arguments: Option[Seq[ByteString]] = None,
      contractCode: Option[ByteString]
  )

  case class CallRequest(tx: CallTx, block: BlockParam)
  case class CallResponse(returnData: ByteString)
  case class IeleCallRequest(tx: IeleCallTx, block: BlockParam)
  case class IeleCallResponse(returnData: Seq[ByteString])
  case class EstimateGasResponse(gas: BigInt)

  case class GetCodeRequest(address: Address, block: BlockParam)
  case class GetCodeResponse(result: ByteString)

  case class GetBalanceRequest(address: Address, block: BlockParam)
  case class GetBalanceResponse(value: BigInt)

  case class GetStorageAtRequest(address: Address, position: BigInt, block: BlockParam)
  case class GetStorageAtResponse(value: ByteString)

  case class ResolvedBlock(block: Block, pendingState: Option[InMemoryWorldStateProxy])

  case class GetTransactionCountRequest(address: Address, block: BlockParam)
  case class GetTransactionCountResponse(value: BigInt)

  case class NewFilterRequest(filter: Filter)
  case class Filter(
      fromBlock: Option[BlockParam],
      toBlock: Option[BlockParam],
      address: Option[Address],
      topics: Seq[Seq[ByteString]]
  )

  case class NewBlockFilterRequest()
  case class NewPendingTransactionFilterRequest()

  case class NewFilterResponse(filterId: BigInt)

  case class UninstallFilterRequest(filterId: BigInt)
  case class UninstallFilterResponse(success: Boolean)

  case class GetFilterChangesRequest(filterId: BigInt)
  case class GetFilterChangesResponse(filterChanges: FilterChanges)

  case class GetFilterLogsRequest(filterId: BigInt)
  case class GetFilterLogsResponse(filterLogs: FilterLogs)

  case class GetLogsRequest(filter: Filter)
  case class GetLogsResponse(filterLogs: LogFilterLogs)

  case class GetStorageRootRequest(address: Address, block: BlockParam)
  case class GetStorageRootResponse(storageRoot: ByteString)

  case class EthPendingTransactionsRequest()
  case class EthPendingTransactionsResponse(pendingTransactions: Seq[PendingTransaction])

  def resolveBlock(
      blockParam: BlockParam,
      blockchain: Blockchain,
      blockGenerator: BlockGenerator
  ): Either[JsonRpcError, ResolvedBlock] = {
    def getBlock(number: BigInt): Either[JsonRpcError, Block] = {
      blockchain
        .getBlockByNumber(number)
        .map(Right.apply)
        .getOrElse(Left(JsonRpcError.InvalidParams(s"Block $number not found")))
    }

    blockParam match {
      case BlockParam.WithNumber(blockNumber) => getBlock(blockNumber).map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Earliest => getBlock(0).map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Latest => getBlock(blockchain.getBestBlockNumber()).map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Pending =>
        blockGenerator.getPendingBlockAndState
          .map(pb => ResolvedBlock(pb.pendingBlock.block, pendingState = Some(pb.worldState)))
          .map(Right.apply)
          .getOrElse(resolveBlock(BlockParam.Latest, blockchain, blockGenerator)) //Default behavior in other clients
    }
  }
}

class EthService(
    blockchain: Blockchain,
    ledger: Ledger,
    stxLedger: StxLedger,
    keyStore: KeyStore,
    pendingTransactionsManager: ActorRef,
    syncingController: ActorRef,
    filterManager: ActorRef,
    filterConfig: FilterConfig,
    blockchainConfig: BlockchainConfig,
    protocolVersion: Int,
    getTransactionFromPoolTimeout: FiniteDuration,
    askTimeout: Timeout
) extends Logger {

  import EthService._

  val hashRate: ConcurrentMap[ByteString, (BigInt, Date)] = new TrieMap[ByteString, (BigInt, Date)]()
  val lastActive = new AtomicReference[Option[Date]](None)

  private[this] def consensus = ledger.consensus
  private[this] def blockGenerator: BlockGenerator = consensus.blockGenerator
  private[this] def fullConsensusConfig = consensus.config

  def protocolVersion(req: ProtocolVersionRequest): ServiceResponse[ProtocolVersionResponse] =
    Task.now(Right(ProtocolVersionResponse(f"0x$protocolVersion%x")))

  /**
    * Implements the eth_getRawTransactionByHash - fetch raw transaction data of a transaction with the given hash.
    *
    * The tx requested will be fetched from the pending tx pool or from the already executed txs (depending on the tx state)
    *
    * @param req with the tx requested (by it's hash)
    * @return the raw transaction hask or None if the client doesn't have the tx
    */
  def getRawTransactionByHash(req: GetTransactionByHashRequest): ServiceResponse[RawTransactionResponse] = {
    getTransactionDataByHash(req.txHash).map(asRawTransactionResponse)
  }

  private def asRawTransactionResponse(txResponse: Option[TransactionData]): Right[Nothing, RawTransactionResponse] =
    Right(RawTransactionResponse(txResponse.map(_.stx)))

  /**
    * Implements the eth_getTransactionByHash method that fetches a requested tx.
    * The tx requested will be fetched from the pending tx pool or from the already executed txs (depending on the tx state)
    *
    * @param req with the tx requested (by it's hash)
    * @return the tx requested or None if the client doesn't have the tx
    */
  def getTransactionByHash(req: GetTransactionByHashRequest): ServiceResponse[GetTransactionByHashResponse] = {
    val eventualMaybeData = getTransactionDataByHash(req.txHash)
    eventualMaybeData.map(txResponse => Right(GetTransactionByHashResponse(txResponse.map(TransactionResponse(_)))))
  }

  def getTransactionDataByHash(txHash: ByteString): Task[Option[TransactionData]] = {
    val maybeTxPendingResponse: Task[Option[TransactionData]] = getTransactionsFromPool.map {
      _.pendingTransactions.map(_.stx.tx).find(_.hash == txHash).map(TransactionData(_))
    }

    maybeTxPendingResponse.map { txPending =>
      txPending.orElse {
        for {
          TransactionLocation(blockHash, txIndex) <- blockchain.getTransactionLocation(txHash)
          Block(header, body) <- blockchain.getBlockByHash(blockHash)
          stx <- body.transactionList.lift(txIndex)
        } yield TransactionData(stx, Some(header), Some(txIndex))
      }
    }
  }

  def getTransactionReceipt(req: GetTransactionReceiptRequest): ServiceResponse[GetTransactionReceiptResponse] =
    Task {
      val result: Option[TransactionReceiptResponse] = for {
        TransactionLocation(blockHash, txIndex) <- blockchain.getTransactionLocation(req.txHash)
        Block(header, body) <- blockchain.getBlockByHash(blockHash)
        stx <- body.transactionList.lift(txIndex)
        receipts <- blockchain.getReceiptsByHash(blockHash)
        receipt: Receipt <- receipts.lift(txIndex)
        // another possibility would be to throw an exception and fail hard, as if we cannot calculate sender for transaction
        // included in blockchain it means that something is terribly wrong
        sender <- SignedTransaction.getSender(stx)
      } yield {

        val gasUsed =
          if (txIndex == 0) receipt.cumulativeGasUsed
          else receipt.cumulativeGasUsed - receipts(txIndex - 1).cumulativeGasUsed

        TransactionReceiptResponse(
          receipt = receipt,
          stx = stx,
          signedTransactionSender = sender,
          transactionIndex = txIndex,
          blockHeader = header,
          gasUsedByTransaction = gasUsed
        )
      }

      Right(GetTransactionReceiptResponse(result))
    }

  /**
    * eth_getTransactionByBlockHashAndIndex that returns information about a transaction by block hash and
    * transaction index position.
    *
    * @return the tx requested or None if the client doesn't have the block or if there's no tx in the that index
    */
  def getTransactionByBlockHashAndIndex(
      req: GetTransactionByBlockHashAndIndexRequest
  ): ServiceResponse[GetTransactionByBlockHashAndIndexResponse] =
    getTransactionByBlockHashAndIndex(req.blockHash, req.transactionIndex)
      .map(td => Right(GetTransactionByBlockHashAndIndexResponse(td.map(TransactionResponse(_)))))

  /**
    * eth_getRawTransactionByBlockHashAndIndex returns raw transaction data of a transaction with the block hash and index of which it was mined
    *
    * @return the tx requested or None if the client doesn't have the block or if there's no tx in the that index
    */
  def getRawTransactionByBlockHashAndIndex(
      req: GetTransactionByBlockHashAndIndexRequest
  ): ServiceResponse[RawTransactionResponse] =
    getTransactionByBlockHashAndIndex(req.blockHash, req.transactionIndex)
      .map(asRawTransactionResponse)

  private def getTransactionByBlockHashAndIndex(blockHash: ByteString, transactionIndex: BigInt) =
    Task {
      for {
        blockWithTx <- blockchain.getBlockByHash(blockHash)
        blockTxs = blockWithTx.body.transactionList if transactionIndex >= 0 && transactionIndex < blockTxs.size
        transaction <- blockTxs.lift(transactionIndex.toInt)
      } yield TransactionData(transaction, Some(blockWithTx.header), Some(transactionIndex.toInt))
    }

  def getGetGasPrice(req: GetGasPriceRequest): ServiceResponse[GetGasPriceResponse] = {
    val blockDifference = 30
    val bestBlock = blockchain.getBestBlockNumber()

    Task {
      val gasPrice = ((bestBlock - blockDifference) to bestBlock)
        .flatMap(blockchain.getBlockByNumber)
        .flatMap(_.body.transactionList)
        .map(_.tx.gasPrice)
      if (gasPrice.nonEmpty) {
        val avgGasPrice = gasPrice.sum / gasPrice.length
        Right(GetGasPriceResponse(avgGasPrice))
      } else {
        Right(GetGasPriceResponse(0))
      }
    }
  }

  // TODO This seems to be re-implemented in TransactionPicker, probably move to a better place? Also generalize the error message.
  private[jsonrpc] val getTransactionsFromPool: Task[PendingTransactionsResponse] = {
    implicit val timeout: Timeout = Timeout(getTransactionFromPoolTimeout)

    pendingTransactionsManager
      .askFor[PendingTransactionsResponse](PendingTransactionsManager.GetPendingTransactions)
      .onErrorRecoverWith { case ex: Throwable =>
        log.error("Failed to get pending transactions, passing empty transactions list", ex)
        Task.now(PendingTransactionsResponse(Nil))
      }
  }

  /**
    * Implements the eth_syncing method that returns syncing information if the node is syncing.
    *
    * @return The syncing status if the node is syncing or None if not
    */
  def syncing(req: SyncingRequest): ServiceResponse[SyncingResponse] =
    syncingController
      .askFor(SyncProtocol.GetStatus)(timeout = askTimeout, implicitly[ClassTag[SyncProtocol.Status]])
      .map {
        case Status.Syncing(startingBlockNumber, blocksProgress, maybeStateNodesProgress) =>
          val stateNodesProgress = maybeStateNodesProgress.getOrElse(Progress.empty)
          SyncingResponse(
            Some(
              SyncingStatus(
                startingBlock = startingBlockNumber,
                currentBlock = blocksProgress.current,
                highestBlock = blocksProgress.target,
                knownStates = stateNodesProgress.target,
                pulledStates = stateNodesProgress.current
              )
            )
          )
        case Status.NotSyncing => SyncingResponse(None)
        case Status.SyncDone => SyncingResponse(None)
      }
      .map(_.asRight)

  def sendRawTransaction(req: SendRawTransactionRequest): ServiceResponse[SendRawTransactionResponse] = {
    import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionDec

    Try(req.data.toArray.toSignedTransaction) match {
      case Success(signedTransaction) =>
        if (SignedTransaction.getSender(signedTransaction).isDefined) {
          pendingTransactionsManager ! PendingTransactionsManager.AddOrOverrideTransaction(signedTransaction)
          Task.now(Right(SendRawTransactionResponse(signedTransaction.hash)))
        } else {
          Task.now(Left(JsonRpcError.InvalidRequest))
        }
      case Failure(_) =>
        Task.now(Left(JsonRpcError.InvalidRequest))
    }
  }

  def call(req: CallRequest): ServiceResponse[CallResponse] = {
    Task {
      doCall(req)(stxLedger.simulateTransaction).map(r => CallResponse(r.vmReturnData))
    }
  }

  def ieleCall(req: IeleCallRequest): ServiceResponse[IeleCallResponse] = {
    import req.tx

    val args = tx.arguments.getOrElse(Nil)
    val dataEither = (tx.function, tx.contractCode) match {
      case (Some(function), None) => Right(rlp.encode(RLPList(function, args)))
      case (None, Some(contractCode)) => Right(rlp.encode(RLPList(contractCode, args)))
      case _ => Left(JsonRpcError.InvalidParams("Iele transaction should contain either functionName or contractCode"))
    }

    dataEither match {
      case Right(data) =>
        call(CallRequest(CallTx(tx.from, tx.to, tx.gas, tx.gasPrice, tx.value, ByteString(data)), req.block))
          .map(_.map { callResponse =>
            IeleCallResponse(
              rlp.decode[Seq[ByteString]](callResponse.returnData.toArray[Byte])(seqEncDec[ByteString]())
            )
          })
      case Left(error) => Task.now(Left(error))
    }
  }

  def estimateGas(req: CallRequest): ServiceResponse[EstimateGasResponse] = {
    Task {
      doCall(req)(stxLedger.binarySearchGasEstimation).map(gasUsed => EstimateGasResponse(gasUsed))
    }
  }

  def getCode(req: GetCodeRequest): ServiceResponse[GetCodeResponse] = {
    Task {
      resolveBlock(req.block, blockchain, blockGenerator).map { case ResolvedBlock(block, _) =>
        val world = blockchain.getWorldStateProxy(
          block.header.number,
          blockchainConfig.accountStartNonce,
          block.header.stateRoot,
          noEmptyAccounts = false,
          ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
        )
        GetCodeResponse(world.getCode(req.address))
      }
    }
  }

  /**
    * eth_getTransactionByBlockNumberAndIndex Returns the information about a transaction with
    * the block number and index of which it was mined.
    *
    * @param req block number and index
    * @return transaction
    */
  def getTransactionByBlockNumberAndIndex(
      req: GetTransactionByBlockNumberAndIndexRequest
  ): ServiceResponse[GetTransactionByBlockNumberAndIndexResponse] = Task {
    getTransactionDataByBlockNumberAndIndex(req.block, req.transactionIndex)
      .map(_.map(TransactionResponse(_)))
      .map(GetTransactionByBlockNumberAndIndexResponse)
  }

  /**
    * eth_getRawTransactionByBlockNumberAndIndex Returns raw transaction data of a transaction
    * with the block number and index of which it was mined.
    *
    * @param req block number and ordering in which a transaction is mined within its block
    * @return raw transaction data
    */
  def getRawTransactionByBlockNumberAndIndex(
      req: GetTransactionByBlockNumberAndIndexRequest
  ): ServiceResponse[RawTransactionResponse] = Task {
    getTransactionDataByBlockNumberAndIndex(req.block, req.transactionIndex)
      .map(x => x.map(_.stx))
      .map(RawTransactionResponse)
  }

  private def getTransactionDataByBlockNumberAndIndex(block: BlockParam, transactionIndex: BigInt) = {
    resolveBlock(block, blockchain, blockGenerator)
      .map { blockWithTx =>
        val blockTxs = blockWithTx.block.body.transactionList
        if (transactionIndex >= 0 && transactionIndex < blockTxs.size)
          Some(
            TransactionData(
              blockTxs(transactionIndex.toInt),
              Some(blockWithTx.block.header),
              Some(transactionIndex.toInt)
            )
          )
        else None
      }
      .left
      .flatMap(_ => Right(None))
  }

  def getBalance(req: GetBalanceRequest): ServiceResponse[GetBalanceResponse] =
    withAccount(req.address, req.block) { account =>
      GetBalanceResponse(account.balance)
    }

  def getStorageAt(req: GetStorageAtRequest): ServiceResponse[GetStorageAtResponse] =
    withAccount(req.address, req.block) { account =>
      GetStorageAtResponse(
        blockchain.getAccountStorageAt(account.storageRoot, req.position, blockchainConfig.ethCompatibleStorage)
      )
    }

  def getTransactionCount(req: GetTransactionCountRequest): ServiceResponse[GetTransactionCountResponse] =
    withAccount(req.address, req.block) { account =>
      GetTransactionCountResponse(account.nonce)
    }

  def newFilter(req: NewFilterRequest): ServiceResponse[NewFilterResponse] = {
    implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)

    import req.filter._
    filterManager
      .askFor[FM.NewFilterResponse](FM.NewLogFilter(fromBlock, toBlock, address, topics))
      .map { resp =>
        Right(NewFilterResponse(resp.id))
      }
  }

  def newBlockFilter(req: NewBlockFilterRequest): ServiceResponse[NewFilterResponse] = {
    implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)
    filterManager
      .askFor[FM.NewFilterResponse](FM.NewBlockFilter)
      .map { resp =>
        Right(NewFilterResponse(resp.id))
      }
  }

  def newPendingTransactionFilter(req: NewPendingTransactionFilterRequest): ServiceResponse[NewFilterResponse] = {
    implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)
    filterManager
      .askFor[FM.NewFilterResponse](FM.NewPendingTransactionFilter)
      .map { resp =>
        Right(NewFilterResponse(resp.id))
      }
  }

  def uninstallFilter(req: UninstallFilterRequest): ServiceResponse[UninstallFilterResponse] = {
    implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)

    filterManager
      .askFor[FM.UninstallFilterResponse.type](FM.UninstallFilter(req.filterId))
      .map(_ => Right(UninstallFilterResponse(success = true)))
  }

  def getFilterChanges(req: GetFilterChangesRequest): ServiceResponse[GetFilterChangesResponse] = {
    implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)

    filterManager
      .askFor[FM.FilterChanges](FM.GetFilterChanges(req.filterId))
      .map { filterChanges =>
        Right(GetFilterChangesResponse(filterChanges))
      }
  }

  def getFilterLogs(req: GetFilterLogsRequest): ServiceResponse[GetFilterLogsResponse] = {
    implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)
    filterManager
      .askFor[FM.FilterLogs](FM.GetFilterLogs(req.filterId))
      .map { filterLogs =>
        Right(GetFilterLogsResponse(filterLogs))
      }
  }

  def getLogs(req: GetLogsRequest): ServiceResponse[GetLogsResponse] = {
    implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)
    import req.filter._

    filterManager
      .askFor[FM.LogFilterLogs](FM.GetLogs(fromBlock, toBlock, address, topics))
      .map { filterLogs =>
        Right(GetLogsResponse(filterLogs))
      }
  }

  private def withAccount[T](address: Address, blockParam: BlockParam)(makeResponse: Account => T): ServiceResponse[T] =
    Task {
      resolveBlock(blockParam, blockchain, blockGenerator)
        .map { case ResolvedBlock(block, _) =>
          blockchain
            .getAccount(address, block.header.number)
            .getOrElse(Account.empty(blockchainConfig.accountStartNonce))
        }
        .map(makeResponse)
    }.onErrorRecover { case _: MissingNodeException =>
      Left(JsonRpcError.NodeNotFound)
    }

  private def doCall[A](req: CallRequest)(
      f: (SignedTransactionWithSender, BlockHeader, Option[InMemoryWorldStateProxy]) => A
  ): Either[JsonRpcError, A] = for {
    stx <- prepareTransaction(req)
    block <- resolveBlock(req.block, blockchain, blockGenerator)
  } yield f(stx, block.block.header, block.pendingState)

  private def getGasLimit(req: CallRequest): Either[JsonRpcError, BigInt] =
    if (req.tx.gas.isDefined) Right[JsonRpcError, BigInt](req.tx.gas.get)
    else resolveBlock(BlockParam.Latest, blockchain, blockGenerator).map(r => r.block.header.gasLimit)

  private def prepareTransaction(req: CallRequest): Either[JsonRpcError, SignedTransactionWithSender] = {
    getGasLimit(req).map { gasLimit =>
      val fromAddress = req.tx.from
        .map(Address.apply) // `from` param, if specified
        .getOrElse(
          keyStore
            .listAccounts()
            .getOrElse(Nil)
            .headOption // first account, if exists and `from` param not specified
            .getOrElse(Address(0))
        ) // 0x0 default

      val toAddress = req.tx.to.map(Address.apply)

      val tx = Transaction(0, req.tx.gasPrice, gasLimit, toAddress, req.tx.value, req.tx.data)
      val fakeSignature = ECDSASignature(0, 0, 0.toByte)
      SignedTransactionWithSender(tx, fakeSignature, fromAddress)
    }
  }

  def getStorageRoot(req: GetStorageRootRequest): ServiceResponse[GetStorageRootResponse] =
    withAccount(req.address, req.block) { account =>
      GetStorageRootResponse(account.storageRoot)
    }

  /**
    * Returns the transactions that are pending in the transaction pool and have a from address that is one of the accounts this node manages.
    *
    * @param req request
    * @return pending transactions
    */
  def ethPendingTransactions(req: EthPendingTransactionsRequest): ServiceResponse[EthPendingTransactionsResponse] =
    getTransactionsFromPool.map { resp =>
      Right(EthPendingTransactionsResponse(resp.pendingTransactions))
    }
}
