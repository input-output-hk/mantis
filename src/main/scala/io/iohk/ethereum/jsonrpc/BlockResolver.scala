package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.consensus.blocks.BlockGenerator
import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.jsonrpc.EthService.{BlockParam, ResolvedBlock}

class BlockResolver(blockchain: Blockchain, blockGenerator: BlockGenerator) {

  def resolveBlock(blockParam: BlockParam): Either[JsonRpcError, ResolvedBlock] = {
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

  private def getBlock(number: BigInt): Either[JsonRpcError, Block] = {
    blockchain
      .getBlockByNumber(number)
      .map(Right.apply)
      .getOrElse(Left(JsonRpcError.InvalidParams(s"Block $number not found")))
  }
}
