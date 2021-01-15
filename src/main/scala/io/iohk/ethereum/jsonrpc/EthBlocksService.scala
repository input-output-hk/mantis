package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.jsonrpc.EthService.{BlockParam, ResolvedBlock}
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import monix.eval.Task
import org.bouncycastle.util.encoders.Hex

//eth_call -> ethService.call. // not moved

object EthBlocksService {
  case class ChainIdRequest()
  case class ChainIdResponse(value: Byte)

  case class BestBlockNumberRequest()
  case class BestBlockNumberResponse(bestBlockNumber: BigInt)

  case class TxCountByBlockHashRequest(blockHash: ByteString)
  case class TxCountByBlockHashResponse(txsQuantity: Option[Int])

  case class BlockByBlockHashRequest(blockHash: ByteString, fullTxs: Boolean)
  case class BlockByBlockHashResponse(blockResponse: Option[BlockResponse])

  case class BlockByNumberRequest(block: BlockParam, fullTxs: Boolean)
  case class BlockByNumberResponse(blockResponse: Option[BlockResponse])

  case class GetBlockTransactionCountByNumberRequest(block: BlockParam)
  case class GetBlockTransactionCountByNumberResponse(result: BigInt)

  case class UncleByBlockHashAndIndexRequest(blockHash: ByteString, uncleIndex: BigInt)
  case class UncleByBlockHashAndIndexResponse(uncleBlockResponse: Option[BlockResponse])

  case class UncleByBlockNumberAndIndexRequest(block: BlockParam, uncleIndex: BigInt)
  case class UncleByBlockNumberAndIndexResponse(uncleBlockResponse: Option[BlockResponse])

  case class GetUncleCountByBlockNumberRequest(block: BlockParam)
  case class GetUncleCountByBlockNumberResponse(result: BigInt)

  case class GetUncleCountByBlockHashRequest(blockHash: ByteString)
  case class GetUncleCountByBlockHashResponse(result: BigInt)
}

class EthBlocksService(blockchain: Blockchain, ledger: Ledger, blockchainConfig: BlockchainConfig) extends Logger {
  import EthBlocksService._

  private[jsonrpc] def consensus = ledger.consensus
  private[jsonrpc] def blockGenerator = consensus.blockGenerator

  def chainId(req: ChainIdRequest): ServiceResponse[ChainIdResponse] =
    Task.now(Right(ChainIdResponse(blockchainConfig.chainId)))

  /**
    * eth_blockNumber that returns the number of most recent block.
    *
    * @return Current block number the client is on.
    */
  def bestBlockNumber(req: BestBlockNumberRequest): ServiceResponse[BestBlockNumberResponse] = Task {
    Right(BestBlockNumberResponse(blockchain.getBestBlockNumber()))
  }

  /**
    * Implements the eth_getBlockTransactionCountByHash method that fetches the number of txs that a certain block has.
    *
    * @param request with the hash of the block requested
    * @return the number of txs that the block has or None if the client doesn't have the block requested
    */
  def getBlockTransactionCountByHash(request: TxCountByBlockHashRequest): ServiceResponse[TxCountByBlockHashResponse] =
    Task {
      val txsCount = blockchain.getBlockBodyByHash(request.blockHash).map(_.transactionList.size)
      Right(TxCountByBlockHashResponse(txsCount))
    }

  /**
    * Implements the eth_getBlockByHash method that fetches a requested block.
    *
    * @param request with the hash of the block requested
    * @return the block requested or None if the client doesn't have the block
    */
  def getByBlockHash(request: BlockByBlockHashRequest): ServiceResponse[BlockByBlockHashResponse] = Task {
    val BlockByBlockHashRequest(blockHash, fullTxs) = request
    val blockOpt = blockchain.getBlockByHash(blockHash)
    val weight = blockchain.getChainWeightByHash(blockHash)

    val blockResponseOpt = blockOpt.map(block => BlockResponse(block, weight, fullTxs = fullTxs))
    Right(BlockByBlockHashResponse(blockResponseOpt))
  }

  /**
    * Implements the eth_getBlockByNumber method that fetches a requested block.
    *
    * @param request with the block requested (by it's number or by tag)
    * @return the block requested or None if the client doesn't have the block
    */
  def getBlockByNumber(request: BlockByNumberRequest): ServiceResponse[BlockByNumberResponse] = Task {
    val BlockByNumberRequest(blockParam, fullTxs) = request
    val blockResponseOpt = EthService.resolveBlock(blockParam, blockchain, blockGenerator).toOption.map {
      case ResolvedBlock(block, pending) =>
        val weight = blockchain.getChainWeightByHash(block.header.hash)
        BlockResponse(block, weight, fullTxs = fullTxs, pendingBlock = pending.isDefined)
    }
    Right(BlockByNumberResponse(blockResponseOpt))
  }

  def getBlockTransactionCountByNumber(
      req: GetBlockTransactionCountByNumberRequest
  ): ServiceResponse[GetBlockTransactionCountByNumberResponse] = {
    Task {
      EthService.resolveBlock(req.block, blockchain, blockGenerator).map { case ResolvedBlock(block, _) =>
        GetBlockTransactionCountByNumberResponse(block.body.transactionList.size)
      }
    }
  }

  /**
    * Implements the eth_getUncleByBlockHashAndIndex method that fetches an uncle from a certain index in a requested block.
    *
    * @param request with the hash of the block and the index of the uncle requested
    * @return the uncle that the block has at the given index or None if the client doesn't have the block or if there's no uncle in that index
    */
  def getUncleByBlockHashAndIndex(
      request: UncleByBlockHashAndIndexRequest
  ): ServiceResponse[UncleByBlockHashAndIndexResponse] = Task {
    val UncleByBlockHashAndIndexRequest(blockHash, uncleIndex) = request
    val uncleHeaderOpt = blockchain
      .getBlockBodyByHash(blockHash)
      .flatMap { body =>
        if (uncleIndex >= 0 && uncleIndex < body.uncleNodesList.size)
          Some(body.uncleNodesList.apply(uncleIndex.toInt))
        else
          None
      }
    val weight = uncleHeaderOpt.flatMap(uncleHeader => blockchain.getChainWeightByHash(uncleHeader.hash))

    //The block in the response will not have any txs or uncles
    val uncleBlockResponseOpt = uncleHeaderOpt.map { uncleHeader =>
      BlockResponse(blockHeader = uncleHeader, weight = weight, pendingBlock = false)
    }
    Right(UncleByBlockHashAndIndexResponse(uncleBlockResponseOpt))
  }

  /**
    * Implements the eth_getUncleByBlockNumberAndIndex method that fetches an uncle from a certain index in a requested block.
    *
    * @param request with the number/tag of the block and the index of the uncle requested
    * @return the uncle that the block has at the given index or None if the client doesn't have the block or if there's no uncle in that index
    */
  def getUncleByBlockNumberAndIndex(
      request: UncleByBlockNumberAndIndexRequest
  ): ServiceResponse[UncleByBlockNumberAndIndexResponse] = Task {
    val UncleByBlockNumberAndIndexRequest(blockParam, uncleIndex) = request
    val uncleBlockResponseOpt = EthService
      .resolveBlock(blockParam, blockchain, blockGenerator)
      .toOption
      .flatMap { case ResolvedBlock(block, pending) =>
        if (uncleIndex >= 0 && uncleIndex < block.body.uncleNodesList.size) {
          val uncleHeader = block.body.uncleNodesList.apply(uncleIndex.toInt)
          val weight = blockchain.getChainWeightByHash(uncleHeader.hash)

          //The block in the response will not have any txs or uncles
          Some(
            BlockResponse(
              blockHeader = uncleHeader,
              weight = weight,
              pendingBlock = pending.isDefined
            )
          )
        } else
          None
      }

    Right(UncleByBlockNumberAndIndexResponse(uncleBlockResponseOpt))
  }

  def getUncleCountByBlockNumber(
      req: GetUncleCountByBlockNumberRequest
  ): ServiceResponse[GetUncleCountByBlockNumberResponse] = {
    Task {
      EthService.resolveBlock(req.block, blockchain, blockGenerator).map { case ResolvedBlock(block, _) =>
        GetUncleCountByBlockNumberResponse(block.body.uncleNodesList.size)
      }
    }
  }

  def getUncleCountByBlockHash(
      req: GetUncleCountByBlockHashRequest
  ): ServiceResponse[GetUncleCountByBlockHashResponse] = {
    Task {
      blockchain.getBlockBodyByHash(req.blockHash) match {
        case Some(blockBody) =>
          Right(GetUncleCountByBlockHashResponse(blockBody.uncleNodesList.size))
        case None =>
          Left(
            JsonRpcError.InvalidParams(s"Block with hash ${Hex.toHexString(req.blockHash.toArray[Byte])} not found")
          )
      }
    }
  }
}
