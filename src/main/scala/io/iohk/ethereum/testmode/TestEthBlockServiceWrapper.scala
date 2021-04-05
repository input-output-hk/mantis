package io.iohk.ethereum.testmode

import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.jsonrpc.EthBlocksService.{BlockByBlockHashResponse, BlockByNumberResponse}
import io.iohk.ethereum.jsonrpc.{
  BaseBlockResponse,
  BaseTransactionResponse,
  EthBlockResponse,
  EthBlocksService,
  EthTransactionResponse,
  ServiceResponse,
  TransactionData
}
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
      _.map(blockByBlockResponse => {
        val fullBlock = blockchain.getBlockByNumber(blockByBlockResponse.blockResponse.get.number).get
        BlockByBlockHashResponse(
          blockByBlockResponse.blockResponse.map(response => toEthResponse(fullBlock, response))
        )
      })
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
      _.map(blockByBlockResponse => {
        val fullBlock = blockchain.getBlockByNumber(blockByBlockResponse.blockResponse.get.number).get
        BlockByNumberResponse(
          blockByBlockResponse.blockResponse
            .map(response => toEthResponse(fullBlock, response))
        )
      })
    )

  private def toEthResponse(block: Block, response: BaseBlockResponse) = EthBlockResponse(
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
    toEthTransaction(block, response.transactions),
    response.uncles
  )

  private def toEthTransaction(
      block: Block,
      responseTransactions: Either[Seq[ByteString], Seq[BaseTransactionResponse]]
  ): Either[Seq[ByteString], Seq[BaseTransactionResponse]] = responseTransactions.map(_ => {
    block.body.transactionList.zipWithIndex.map { case (stx, transactionIndex) =>
      EthTransactionResponse(tx = TransactionData(stx, Some(block.header), Some(transactionIndex)))
    }
  })
}
