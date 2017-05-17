package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Blockchain, SignedTransaction}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.transactions.PendingTransactionsManager

import scala.concurrent.{ExecutionContext, Future}

object EthService {

  val CurrentProtocolVersion = 63

  case class ProtocolVersionRequest()
  case class ProtocolVersionResponse(value: String)

  case class TxCountByBlockHashRequest(blockHash: ByteString)
  case class TxCountByBlockHashResponse(txsQuantity: Option[Int])

  case class BlockByBlockHashRequest(blockHash: ByteString, fullTxs: Boolean)
  case class BlockByBlockHashResponse(blockResponse: Option[BlockResponse])

  case class UncleByBlockHashAndIndexRequest(blockHash: ByteString, uncleIndex: BigInt)
  case class UncleByBlockHashAndIndexResponse(uncleBlockResponse: Option[BlockResponse])

  case class SyncingRequest()
  case class SyncingResponse(startingBlock: BigInt, currentBlock: BigInt, highestBlock: BigInt)

  case class SendRawTransactionRequest(data: ByteString)
  case class SendRawTransactionResponse(transactionHash: ByteString)
}

class EthService(blockchain: Blockchain, appStateStorage: AppStateStorage, pendingTransactionsManager: ActorRef) {

  import EthService._

  def protocolVersion(req: ProtocolVersionRequest): Future[ProtocolVersionResponse] =
    Future.successful(ProtocolVersionResponse(f"0x$CurrentProtocolVersion%x"))

  /**
    * Implements the eth_getBlockTransactionCountByHash method that fetches the number of txs that a certain block has.
    *
    * @param request with the hash of the block requested
    * @return the number of txs that the block has or None if the client doesn't have the block requested
    */
  def getBlockTransactionCountByHash(request: TxCountByBlockHashRequest)
                                    (implicit executor: ExecutionContext): Future[TxCountByBlockHashResponse] = Future {
    val txsCount = blockchain.getBlockBodyByHash(request.blockHash).map(_.transactionList.size)
    TxCountByBlockHashResponse(txsCount)
  }

  /**
    * Implements the eth_getBlockByHash method that fetches a requested block.
    *
    * @param request with the hash of the block requested
    * @return the block requested or None if the client doesn't have the block
    */
  def getByBlockHash(request: BlockByBlockHashRequest)
                    (implicit executor: ExecutionContext): Future[BlockByBlockHashResponse] = Future {
    val BlockByBlockHashRequest(blockHash, fullTxs) = request
    val blockOpt = blockchain.getBlockByHash(blockHash)
    val totalDifficulty = blockchain.getTotalDifficultyByHash(blockHash)

    val blockResponseOpt = blockOpt.map(block => BlockResponse(block, fullTxs, totalDifficulty))
    BlockByBlockHashResponse(blockResponseOpt)
  }

  /**
    * Implements the eth_getUncleByBlockHashAndIndex method that fetches an uncle from a certain index in a requested block.
    *
    * @param request with the hash of the block and the index of the uncle requested
    * @return the uncle that the block has at the given index or None if the client doesn't have the block or if there's no uncle in that index
    */
  def getUncleByBlockHashAndIndex(request: UncleByBlockHashAndIndexRequest)
                                 (implicit executor: ExecutionContext): Future[UncleByBlockHashAndIndexResponse] = Future {
    val UncleByBlockHashAndIndexRequest(blockHash, uncleIndex) = request
    val uncleHeaderOpt = blockchain.getBlockBodyByHash(blockHash)
      .flatMap { body =>
        if (uncleIndex >= 0 && uncleIndex < body.uncleNodesList.size)
          Some(body.uncleNodesList.apply(uncleIndex.toInt))
        else
          None
      }
    val totalDifficulty = uncleHeaderOpt.flatMap(uncleHeader => blockchain.getTotalDifficultyByHash(uncleHeader.hash))

    //The block in the response will not have any txs or uncles
    val uncleBlockResponseOpt = uncleHeaderOpt.map { uncleHeader => BlockResponse(blockHeader = uncleHeader, totalDifficulty = totalDifficulty) }
    UncleByBlockHashAndIndexResponse(uncleBlockResponseOpt)
  }

 def syncing(req: SyncingRequest): Future[SyncingResponse] = {
    Future.successful(SyncingResponse(
      startingBlock = appStateStorage.getSyncStartingBlock(),
      currentBlock = appStateStorage.getBestBlockNumber(),
      highestBlock = appStateStorage.getEstimatedHighestBlock()))
  }

  def sendRawTransaction(req: SendRawTransactionRequest): Future[SendRawTransactionResponse] = {
    import io.iohk.ethereum.rlp.RLPImplicitConversions._
    val signedTransaction = SignedTransactions.txRlpEncDec.decode(req.data)
    pendingTransactionsManager ! PendingTransactionsManager.BroadcastTransaction(signedTransaction)
    Future.successful(SendRawTransactionResponse(signedTransaction.hash))
  }

}
