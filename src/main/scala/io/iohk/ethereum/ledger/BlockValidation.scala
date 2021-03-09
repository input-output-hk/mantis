package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError

class BlockValidation(consensus: Consensus, blockchain: Blockchain, blockQueue: BlockQueue) {

  def validateBlockBeforeExecution(block: Block): Either[ValidationBeforeExecError, BlockExecutionSuccess] =
    consensus.validators.validateBlockBeforeExecution(
      block = block,
      getBlockHeaderByHash = getBlockHeaderFromChainOrQueue,
      getNBlocksBack = getNBlocksBackFromChainOrQueue
    )

  private def getBlockHeaderFromChainOrQueue(hash: ByteString): Option[BlockHeader] =
    blockchain.getBlockHeaderByHash(hash).orElse(blockQueue.getBlockByHash(hash).map(_.header))

  private def getNBlocksBackFromChainOrQueue(hash: ByteString, n: Int): List[Block] = {
    val queuedBlocks = blockQueue.getBranch(hash, dequeue = false).takeRight(n)
    if (queuedBlocks.length == n) {
      queuedBlocks
    } else {
      val chainedBlockHash = queuedBlocks.headOption.map(_.header.parentHash).getOrElse(hash)
      blockchain.getBlockByHash(chainedBlockHash) match {
        case None =>
          // The in memory blocks aren't connected to the db ones, we don't have n blocks to return so we return none
          Nil

        case Some(block) =>
          // We already have |block +: queuedBlocks|
          val remaining = n - queuedBlocks.length - 1

          val numbers = (block.header.number - remaining) until block.header.number
          val blocks = (numbers.toList.flatMap(blockchain.getBlockByNumber) :+ block) ::: queuedBlocks
          blocks
      }
    }
  }

  def validateBlockAfterExecution(
      block: Block,
      stateRootHash: ByteString,
      receipts: Seq[Receipt],
      gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess] =
    consensus.validators.validateBlockAfterExecution(
      block = block,
      stateRootHash = stateRootHash,
      receipts = receipts,
      gasUsed = gasUsed
    )
}
