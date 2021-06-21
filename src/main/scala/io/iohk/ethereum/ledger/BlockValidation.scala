package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain, BlockchainReader, Receipt}
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError

class BlockValidation(
    consensus: Consensus,
    blockchain: Blockchain,
    blockchainReader: BlockchainReader,
    blockQueue: BlockQueue
) {

  def validateBlockBeforeExecution(block: Block): Either[ValidationBeforeExecError, BlockExecutionSuccess] = {
    consensus.validators.validateBlockBeforeExecution(
      block = block,
      getBlockHeaderByHash = getBlockHeaderFromChainOrQueue,
      getNBlocksBack = getNBlocksBackFromChainOrQueue
    )
  }

  private def getBlockHeaderFromChainOrQueue(hash: ByteString): Option[BlockHeader] = {
    blockchainReader.getBlockHeaderByHash(hash).orElse(blockQueue.getBlockByHash(hash).map(_.header))
  }

  private def getNBlocksBackFromChainOrQueue(hash: ByteString, n: Int): List[Block] = {
    val queuedBlocks = blockQueue.getBranch(hash, dequeue = false).takeRight(n)
    if (queuedBlocks.length == n) {
      queuedBlocks
    } else {
      val chainedBlockHash = queuedBlocks.headOption.map(_.header.parentHash).getOrElse(hash)
      blockchainReader.getBlockByHash(chainedBlockHash) match {
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
  ): Either[BlockExecutionError, BlockExecutionSuccess] = {
    consensus.validators.validateBlockAfterExecution(
      block = block,
      stateRootHash = stateRootHash,
      receipts = receipts,
      gasUsed = gasUsed
    )
  }
}
