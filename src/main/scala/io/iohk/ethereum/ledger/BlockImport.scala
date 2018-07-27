package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.metrics.{ Metrics, MetricsClient }
import io.iohk.ethereum.utils.{ BlockchainConfig, Logger }
import org.bouncycastle.util.encoders.Hex

class BlockImport(
  blockchain: BlockchainImpl,
  blockQueue: BlockQueue,
  blockchainConfig: BlockchainConfig,
  executeBlock: (Block, Boolean) => Either[BlockExecutionError, Seq[Receipt]]
) extends Logger {

  def importBlockToTop(block: Block, bestBlockNumber: BigInt, currentTd: BigInt): BlockImportResult = {
    val topBlockHash = blockQueue.enqueueBlock(block, bestBlockNumber).get.hash
    val topBlocks = blockQueue.getBranch(topBlockHash, dequeue = true)
    val (importedBlocks, maybeError) = executeBlocks(topBlocks, currentTd)
    val totalDifficulties = importedBlocks.foldLeft(List(currentTd)) {(tds, b) =>
      (tds.head + b.header.difficulty) :: tds
    }.reverse.tail

    val result = maybeError match {
      case None =>
        BlockImportedToTop(importedBlocks, totalDifficulties)

      case Some(error) if importedBlocks.isEmpty =>
        blockQueue.removeSubtree(block.header.hash)
        BlockImportFailed(error.toString)

      case Some(_) =>
        topBlocks.drop(importedBlocks.length).headOption.foreach { failedBlock =>
          blockQueue.removeSubtree(failedBlock.header.hash)
        }
        BlockImportedToTop(importedBlocks, totalDifficulties)
    }

    importedBlocks.foreach { b =>
      log.debug(s"Imported new block (${b.header.number}: ${Hex.toHexString(b.header.hash.toArray)}) to the top of chain")
    }

    if(importedBlocks.nonEmpty) {
      val maxNumber = importedBlocks.map(_.header.number).max
      MetricsClient.get().gauge(Metrics.LedgerImportBlockNumber, maxNumber.toLong)
    }

    result
  }

  /**
    * Used to revert chain reorganisation in the event that one of the blocks from new branch
    * fails to execute
    *
    * @param newBranch - new blocks
    * @param oldBranch - old blocks along with corresponding receipts and totalDifficulties
    * @param executedBlocks - sub-sequence of new branch that was executed correctly
    */
  private def revertChainReorganisation(newBranch: List[Block], oldBranch: List[(Block, Seq[Receipt], BigInt)],
    executedBlocks: List[Block]): Unit = {

    if (executedBlocks.nonEmpty) {
      removeBlocksUntil(executedBlocks.head.header.parentHash, executedBlocks.last.header.number)
    }

    oldBranch.foreach { case (block, receipts, td) =>
      blockchain.save(block, receipts, td, saveAsBestBlock = false)
    }

    val bestNumber = oldBranch.last._1.header.number
    blockchain.saveBestKnownBlock(bestNumber)
    executedBlocks.foreach(blockQueue.enqueueBlock(_, bestNumber))

    newBranch.diff(executedBlocks).headOption.foreach { block =>
      blockQueue.removeSubtree(block.header.hash)
    }
  }

  /**
    * Remove blocks from the [[Blockchain]] along with receipts and total difficulties
    * @param parent remove blocks until this hash (exclusive)
    * @param fromNumber start removing from this number (downwards)
    * @return the list of removed blocks along with receipts and total difficulties
    */
  private def removeBlocksUntil(parent: ByteString, fromNumber: BigInt): List[(Block, Seq[Receipt], BigInt)] = {
    blockchain.getBlockByNumber(fromNumber) match {
      case Some(block) if block.header.hash == parent =>
        Nil

      case Some(block) =>
        val blockHeaderHash = block.header.hash
        val receipts = blockchain.getReceiptsByHash(blockHeaderHash).get
        val td = blockchain.getTotalDifficultyByHash(blockHeaderHash).get

        //not updating best block number for efficiency, it will be updated in the callers anyway
        blockchain.removeBlock(blockHeaderHash, withState = true)
        (block, receipts, td) :: removeBlocksUntil(parent, fromNumber - 1)

      case None =>
        log.error(s"Unexpected missing block number: $fromNumber")
        Nil
    }
  }

  /**
    * Executes a list blocks, storing the results in the blockchain
    * @param blocks block to be executed
    * @return a list of blocks that were correctly executed and an optional [[BlockExecutionError]]
    */
  private def executeBlocks(blocks: List[Block], parentTd: BigInt): (List[Block], Option[BlockExecutionError]) = {
    blocks match {
      case block :: remainingBlocks =>
        executeBlock(block, true) match {
          case Right (receipts) =>
            val td = parentTd + block.header.difficulty
            blockchain.save(block, receipts, td, saveAsBestBlock = true)

            val (executedBlocks, error) = executeBlocks(remainingBlocks, td)
            (block :: executedBlocks, error)

          case Left(error) =>
            (Nil, Some(error))
        }

      case Nil =>
        (Nil, None)
    }
  }

  def enqueueBlockOrReorganiseChain(block: Block, bestBlockHeader: BlockHeader, currentTd: BigInt): BlockImportResult = {
    // compares the total difficulties of branches, and resolves the tie by gas if enabled
    // yes, apparently only the gas from last block is checked:
    // https://github.com/ethereum/cpp-ethereum/blob/develop/libethereum/BlockChain.cpp#L811
    def isBetterBranch(newTd: BigInt): Boolean =
      newTd > currentTd ||
        (blockchainConfig.gasTieBreaker && newTd == currentTd && block.header.gasUsed > bestBlockHeader.gasUsed)

    blockQueue.enqueueBlock(block, bestBlockHeader.number) match {
      case Some(Leaf(leafHash, leafTd)) if isBetterBranch(leafTd) =>
        log.debug("Found a better chain, about to reorganise")
        reorganiseChainFromQueue(leafHash) match {
          case Right((oldBranch, newBranch)) =>
            val totalDifficulties = newBranch.tail.foldRight(List(leafTd)) { (b, tds) =>
              (tds.head - b.header.difficulty) :: tds
            }
            ChainReorganised(oldBranch, newBranch, totalDifficulties)

          case Left(error) =>
            BlockImportFailed(s"Error while trying to reorganise chain: $error")
        }

      case _ =>
        BlockEnqueued
    }
  }

  private def reorganiseChainFromQueue(queuedLeaf: ByteString): Either[BlockExecutionError, (List[Block], List[Block])] = {
    blockchain.persistCachedNodes()
    val newBranch = blockQueue.getBranch(queuedLeaf, dequeue = true)
    val parent = newBranch.head.header.parentHash
    val bestNumber = blockchain.getBestBlockNumber()
    val parentTd = blockchain.getTotalDifficultyByHash(parent).get

    val staleBlocksWithReceiptsAndTDs = removeBlocksUntil(parent, bestNumber).reverse
    val staleBlocks = staleBlocksWithReceiptsAndTDs.map(_._1)

    for (block <- staleBlocks) yield blockQueue.enqueueBlock(block)

    val (executedBlocks, maybeError) = executeBlocks(newBranch, parentTd)
    maybeError match {
      case None =>
        Right(staleBlocks, executedBlocks)

      case Some(error) =>
        revertChainReorganisation(newBranch, staleBlocksWithReceiptsAndTDs, executedBlocks)
        Left(error)
    }
  }
}

sealed trait BlockImportResult
case class BlockImportedToTop(imported: List[Block], totalDifficulties: List[BigInt]) extends BlockImportResult
case object BlockEnqueued extends BlockImportResult
case object DuplicateBlock extends BlockImportResult
case class ChainReorganised(oldBranch: List[Block], newBranch: List[Block], totalDifficulties: List[BigInt]) extends BlockImportResult
case class BlockImportFailed(error: String) extends BlockImportResult
case object UnknownParent extends BlockImportResult
