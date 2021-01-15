package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransaction
import monix.eval.Task
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.transactions.PendingTransactionsManager
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import akka.actor.ActorRef
import scala.concurrent.duration.FiniteDuration
import io.iohk.ethereum.transactions.TransactionPicker
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.Ledger

object EthTxService {
  case class GetTransactionByHashRequest(txHash: ByteString) //rename to match request
  case class GetTransactionByHashResponse(txResponse: Option[TransactionResponse])
  case class GetTransactionByBlockHashAndIndexRequest(blockHash: ByteString, transactionIndex: BigInt)
  case class GetTransactionByBlockHashAndIndexResponse(transactionResponse: Option[TransactionResponse])
  case class GetTransactionByBlockNumberAndIndexRequest(block: BlockParam, transactionIndex: BigInt)
  case class GetTransactionByBlockNumberAndIndexResponse(transactionResponse: Option[TransactionResponse])
  case class GetGasPriceRequest()
  case class GetGasPriceResponse(price: BigInt)
  case class SendRawTransactionRequest(data: ByteString)
  case class SendRawTransactionResponse(transactionHash: ByteString)
  case class EthPendingTransactionsRequest()
  case class EthPendingTransactionsResponse(pendingTransactions: Seq[PendingTransaction])
  case class GetTransactionReceiptRequest(txHash: ByteString)
  case class GetTransactionReceiptResponse(txResponse: Option[TransactionReceiptResponse])
  case class RawTransactionResponse(transactionResponse: Option[SignedTransaction])
  case class ResolvedBlock(block: Block, pendingState: Option[InMemoryWorldStateProxy])

}

class EthTxService(
    blockchain: Blockchain,
    ledger: Ledger,
    val pendingTransactionsManager: ActorRef,
    val getTransactionFromPoolTimeout: FiniteDuration
) extends TransactionPicker {
  import EthTxService._

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

  private def getTransactionDataByHash(txHash: ByteString): Task[Option[TransactionData]] = {
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
    resolveBlock(block)
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

  /** * TODO: duplicated
    */

  private[this] def consensus = ledger.consensus
  private[this] def blockGenerator = consensus.blockGenerator
  private def resolveBlock(blockParam: BlockParam): Either[JsonRpcError, ResolvedBlock] = {
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
          .getOrElse(resolveBlock(BlockParam.Latest)) //Default behavior in other clients
    }
  }

}
