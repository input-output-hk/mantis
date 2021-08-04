package io.iohk.ethereum.ledger

import akka.util.ByteString

import io.iohk.ethereum.consensus.mining.Mining
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.utils.BlockchainConfig

class BlockValidation(
    mining: Mining,
    blockchainReader: BlockchainReader,
    blockQueue: BlockQueue
) {

  def validateBlockBeforeExecution(
      block: Block
  )(implicit blockchainConfig: BlockchainConfig): Either[ValidationBeforeExecError, BlockExecutionSuccess] =
    mining.validators.validateBlockBeforeExecution(
      block = block,
      getBlockHeaderByHash = getBlockHeaderFromChainOrQueue,
      getNBlocksBack = getNBlocksBackFromChainOrQueue
    )

  private def getBlockHeaderFromChainOrQueue(hash: ByteString): Option[BlockHeader] =
    blockchainReader.getBlockHeaderByHash(hash).orElse(blockQueue.getBlockByHash(hash).map(_.header))

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

        case Some(highestBlockInStorage) =>
          // We already have |block +: queuedBlocks|
          val remaining = n - queuedBlocks.length - 1
          val remainingBlocks = Iterator
            .iterate(blockchainReader.getBlockByHash(highestBlockInStorage.header.parentHash))(
              _.filter(_.number > 0) // avoid trying to fetch parent of genesis
                .flatMap(p => blockchainReader.getBlockByHash(p.header.parentHash))
            )
            .take(remaining)
            .collect { case Some(block) => block }
            .toList
          (remainingBlocks :+ highestBlockInStorage) ::: queuedBlocks
      }
    }
  }

  def validateBlockAfterExecution(
      block: Block,
      stateRootHash: ByteString,
      receipts: Seq[Receipt],
      gasUsed: BigInt
  )(implicit blockchainConfig: BlockchainConfig): Either[BlockExecutionError, BlockExecutionSuccess] =
    mining.validators.validateBlockAfterExecution(
      block = block,
      stateRootHash = stateRootHash,
      receipts = receipts,
      gasUsed = gasUsed
    )
}
