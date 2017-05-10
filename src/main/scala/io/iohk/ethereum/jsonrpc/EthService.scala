package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Blockchain, SignedTransaction}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

import scala.concurrent.{ExecutionContext, Future}

object EthService {

  case class TxCountByBlockHashRequest(blockHash: ByteString)
  case class TxCountByBlockHashResponse(txsQuantity: Option[Int])

  case class BlockByBlockHashRequest(blockHash: ByteString, fullTxs: Boolean)
  case class BlockByBlockHashResponse(blockView: Option[BlockResponse])

  case class UncleByBlockHashAndIndexRequest(blockHash: ByteString, uncleIndex: BigInt)
  case class UncleByBlockHashAndIndexResponse(uncleBlockView: Option[BlockResponse])

  case class BlockByNumberRequest()
  case class BlockByNumberResponse(bestBlockNumber: BigInt)

  case class GetTransactionByBlockHashAndIndexRequest(blockHash: ByteString, transactionIndex: BigInt)
  case class GetTransactionByBlockHashAndIndexResponse(transactionResponse: Option[TransactionResponse])

}

class EthService(private val blockchain: Blockchain,
                 private val appStateStorage: AppStateStorage) {

  import EthService._

  /**
    * eth_blockNumber that returns the number of most recent block.
    *
    * @return Current block number the client is on.
    */
  def bestBlockNumber(req: BlockByNumberRequest)(implicit executionContext: ExecutionContext): Future[BlockByNumberResponse] = Future {
    BlockByNumberResponse(appStateStorage.getBestBlockNumber())
  }

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

    val blockViewOpt = blockOpt.map(block => BlockResponse(block, fullTxs, totalDifficulty))
    BlockByBlockHashResponse(blockViewOpt)
  }

  /**
    * eth_getTransactionByBlockHashAndIndex that returns information about a transaction by block hash and
    * transaction index position.
    *
    * @return the tx requested or None if the client doesn't have the block or if there's no tx in the that index
    */
  def getTransactionByBlockHashAndIndexRequest(req: GetTransactionByBlockHashAndIndexRequest)(implicit executionContext: ExecutionContext)
  : Future[GetTransactionByBlockHashAndIndexResponse] = Future {
    import req._
    val maybeTransactionResponse = blockchain.getBlockByHash(blockHash).flatMap{
      blockWithTx =>
        val blockTxs = blockWithTx.body.transactionList
        if (transactionIndex >= 0 && transactionIndex < blockTxs.size)
          Some(TransactionResponse(blockTxs(transactionIndex.toInt), Some(blockWithTx.header), Some(transactionIndex.toInt)))
        else None
    }
    GetTransactionByBlockHashAndIndexResponse(maybeTransactionResponse)
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
      .flatMap{body =>
        if(uncleIndex >=0 && uncleIndex < body.uncleNodesList.size)
          Some(body.uncleNodesList.apply(uncleIndex.toInt))
        else
          None}
    val totalDifficulty = uncleHeaderOpt.flatMap(uncleHeader => blockchain.getTotalDifficultyByHash(uncleHeader.hash))

    //The block in the response will not have any txs or uncles
    val uncleBlockViewOpt = uncleHeaderOpt.map { uncleHeader => BlockResponse(blockHeader = uncleHeader, totalDifficulty = totalDifficulty) }
    UncleByBlockHashAndIndexResponse(uncleBlockViewOpt)
  }
}
