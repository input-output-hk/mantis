package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy

sealed trait BlockParam

object BlockParam {
  case class WithNumber(n: BigInt) extends BlockParam
  case object Latest extends BlockParam
  case object Pending extends BlockParam
  case object Earliest extends BlockParam
}

case class ResolvedBlock(block: Block, pendingState: Option[InMemoryWorldStateProxy])

trait ResolveBlock {
  def blockchain: Blockchain
  def blockchainReader: BlockchainReader
  def consensus: Consensus

  def resolveBlock(blockParam: BlockParam): Either[JsonRpcError, ResolvedBlock] =
    blockParam match {
      case BlockParam.WithNumber(blockNumber) => getBlock(blockNumber).map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Earliest                => getBlock(0).map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Latest                  => getLatestBlock().map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Pending =>
        consensus.blockGenerator.getPendingBlockAndState
          .map(pb => ResolvedBlock(pb.pendingBlock.block, pendingState = Some(pb.worldState)))
          .map(Right.apply)
          .getOrElse(resolveBlock(BlockParam.Latest)) //Default behavior in other clients
    }

  private def getBlock(number: BigInt): Either[JsonRpcError, Block] =
    blockchainReader.getBestBranch
      .getBlockByNumber(number)
      .toRight(JsonRpcError.InvalidParams(s"Block $number not found"))

  private def getLatestBlock(): Either[JsonRpcError, Block] =
    blockchainReader
      .getBestBlock()
      .toRight(JsonRpcError.InvalidParams("Latest block not found"))
}
