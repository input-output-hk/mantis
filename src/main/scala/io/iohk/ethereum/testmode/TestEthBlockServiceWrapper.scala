package io.iohk.ethereum.testmode

import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.jsonrpc.EthBlocksService.{BlockByBlockHashResponse, BlockByNumberResponse}
import io.iohk.ethereum.jsonrpc.{BaseBlockResponse, EthBlockResponse, EthBlocksService, ServiceResponse}
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.consensus.Consensus
import akka.util.ByteString

class TestEthBlockServiceWrapper(blockchain: Blockchain, ledger: Ledger, consensus: Consensus)
    extends EthBlocksService(blockchain, ledger)
    with Logger {

  /**
    * Implements the eth_getBlockByHash method that fetches a requested block.
    *
    * @param request with the hash of the block requested
    * @return the block requested or None if the client doesn't have the block
    */
  override def getByBlockHash(
      request: EthBlocksService.BlockByBlockHashRequest
  ): ServiceResponse[EthBlocksService.BlockByBlockHashResponse] = super
    .getByBlockHash(request)
    .map(
      _.map(blockByBlockResponse =>
        BlockByBlockHashResponse(
          blockByBlockResponse.blockResponse.map(response => toEthResponse(response))
        )
      )
    )

  /**
    * Implements the eth_getBlockByNumber method that fetches a requested block.
    *
    * @param request with the block requested (by it's number or by tag)
    * @return the block requested or None if the client doesn't have the block
    */
  override def getBlockByNumber(
      request: EthBlocksService.BlockByNumberRequest
  ): ServiceResponse[EthBlocksService.BlockByNumberResponse] = super
    .getBlockByNumber(request)
    .map(
      _.map(blockByBlockResponse =>
        BlockByNumberResponse(
          blockByBlockResponse.blockResponse
            .map(response => toEthResponse(response))
        )
      )
    )

  private def toEthResponse(response: BaseBlockResponse) = EthBlockResponse(
    response.number,
    response.hash,
    response.mixHash,
    response.parentHash,
    response.nonce,
    response.sha3Uncles,
    response.logsBloom,
    response.transactionsRoot,
    response.stateRoot,
    response.receiptsRoot,
    response.miner,
    response.difficulty,
    response.totalDifficulty,
    response.extraData,
    response.size,
    response.gasLimit,
    response.gasUsed,
    response.timestamp,
    response.transactions,
    response.uncles
  )
}
