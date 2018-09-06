package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.metrics.{ Metrics, MetricsClient }
import io.iohk.ethereum.utils.{ BlockchainConfig, Logger }
import org.bouncycastle.util.encoders.Hex

import scala.annotation.tailrec

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

    val result = maybeError match {
      case None =>
        BlockImportedToTop(importedBlocks)

      case Some(error) if importedBlocks.isEmpty =>
        blockQueue.removeSubtree(block.header.hash)
        BlockImportFailed(error.toString)

      case Some(_) =>
        topBlocks.drop(importedBlocks.length).headOption.foreach { failedBlock =>
          blockQueue.removeSubtree(failedBlock.header.hash)
        }
        BlockImportedToTop(importedBlocks)
    }

    importedBlocks.foreach { b =>
      log.debug(s"Imported new block (${b.block.header.number}: ${Hex.toHexString(b.block.header.hash.toArray)}) to the top of chain")
    }

    if(importedBlocks.nonEmpty) {
      val maxNumber = importedBlocks.map(_.block.header.number).max
      MetricsClient.get().gauge(Metrics.LedgerImportBlockNumber, maxNumber.toLong)
    }

    result
  }

  /** Used to revert chain reorganisation in the event that one of the blocks from new branch
    * fails to execute
    *
    * @param newBranch      new blocks
    * @param oldBranch      old blocks along with corresponding receipts and totalDifficulties
    * @param executedBlocks sub-sequence of new branch that was executed correctly
    */
  private def revertChainReorganisation(newBranch: List[Block], oldBranch: List[BlockData], executedBlocks: List[BlockData]): Unit = {
    if (executedBlocks.nonEmpty) {
      removeBlocksUntil(executedBlocks.head.block.header.parentHash, executedBlocks.last.block.header.number)
    }

    oldBranch.foreach { data =>
      blockchain.save(data.block, data.receipts, data.td, saveAsBestBlock = false)
    }

    val bestNumber = oldBranch.last.block.header.number
    blockchain.saveBestKnownBlock(bestNumber)
    executedBlocks.foreach(data => blockQueue.enqueueBlock(data.block, bestNumber))

    newBranch.diff(executedBlocks.map(_.block)).headOption.foreach { block =>
      blockQueue.removeSubtree(block.header.hash)
    }
  }

  /** Remove blocks from the [[Blockchain]] along with receipts and total difficulties.
    *
    * @param parent     remove blocks until this hash (exclusive)
    * @param fromNumber start removing from this number (downwards)
    * @return the list of removed blocks along with receipts and total difficulties
    */
  private def removeBlocksUntil(parent: ByteString, fromNumber: BigInt): List[BlockData] = {
    blockchain.getBlockByNumber(fromNumber) match {
      case Some(block) if block.header.hash == parent =>
        Nil

      case Some(block) =>
        val receipts = blockchain.getReceiptsByHash(block.header.hash).get
        val td = blockchain.getTotalDifficultyByHash(block.header.hash).get

        //not updating best block number for efficiency, it will be updated in the callers anyway
        blockchain.removeBlock(block.header.hash, withState = true)
        BlockData(block, receipts, td):: removeBlocksUntil(parent, fromNumber - 1)

      case None =>
        log.error(s"Unexpected missing block number: $fromNumber")
        Nil
    }
  }

  /** Executes a list blocks, storing the results in the blockchain.
    *
    * @param blocks   blocks to be executed
    * @param parentTd transaction difficulty of the parent
    * @return a list of blocks that were correctly executed and an optional [[BlockExecutionError]]
    */
  private def executeBlocks(blocks: List[Block], parentTd: BigInt): (List[BlockData], Option[BlockExecutionError]) = {
    @tailrec
    def go(executedBlocks: List[BlockData], remainingBlocks: List[Block], parentTd: BigInt, error: Option[BlockExecutionError])
    :(List[BlockData], Option[BlockExecutionError]) ={
      if (remainingBlocks.isEmpty) {
        (executedBlocks.reverse, None)
      } else if (error.isDefined) {
        (executedBlocks, error)
      } else {
        val blockToExecute = remainingBlocks.head
        executeBlock(blockToExecute, true) match {
          case Right (receipts) =>
            val td = parentTd + blockToExecute.header.difficulty
            val newBlockData = BlockData(blockToExecute, receipts, td)
            blockchain.save(newBlockData.block, newBlockData.receipts, newBlockData.td, saveAsBestBlock = true)
            go(newBlockData :: executedBlocks, remainingBlocks.tail, td, None)
          case Left(executionError) =>
            go(executedBlocks, remainingBlocks, 0, Some(executionError))
        }
      }
    }

    go(List.empty[BlockData], blocks, parentTd, None)
  }

  def enqueueBlockOrReorganiseChain(block: Block, bestBlockHeader: BlockHeader, currentTd: BigInt): BlockImportResult = {
    // compares the total difficulties of branches, and resolves the tie by gas if enabled
    // yes, apparently only the gas from last block is checked:
    // https://github.com/ethereum/cpp-ethereum/blob/develop/libethereum/BlockChain.cpp#L811
    def isBetterBranch(newTd: BigInt): Boolean =
      newTd > currentTd || (blockchainConfig.gasTieBreaker && newTd == currentTd && block.header.gasUsed > bestBlockHeader.gasUsed)

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

  /** Once a better branch was found this attempts to reorganise the chain
    *
    * @param queuedLeaf a block hash that determines a new branch stored in the queue (newest block from the branch)
    * @return [[BlockExecutionError]] if one of the blocks in the new branch failed to execute, otherwise:
    *        (oldBranch, newBranch) as lists of blocks
    */
  private def reorganiseChainFromQueue(queuedLeaf: ByteString): Either[BlockExecutionError, (List[Block], List[Block])] = {
    blockchain.persistCachedNodes()
    val newBranch = blockQueue.getBranch(queuedLeaf, dequeue = true)
    val parent = newBranch.head.header.parentHash
    val bestNumber = blockchain.getBestBlockNumber()
    val parentTd = blockchain.getTotalDifficultyByHash(parent).get

    val staleBlocksWithReceiptsAndTDs = removeBlocksUntil(parent, bestNumber).reverse
    val staleBlocks = staleBlocksWithReceiptsAndTDs.map(_.block)

    for (block <- staleBlocks) yield blockQueue.enqueueBlock(block)

    val (executedBlocks, maybeError) = executeBlocks(newBranch, parentTd)
    maybeError match {
      case None =>
        Right(staleBlocks, executedBlocks.map(_.block))

      case Some(error) =>
        revertChainReorganisation(newBranch, staleBlocksWithReceiptsAndTDs, executedBlocks)
        Left(error)
    }
  }
}

sealed trait BlockImportResult

case class BlockImportedToTop(blockImportData: List[BlockData]) extends BlockImportResult

case object BlockEnqueued extends BlockImportResult

case object DuplicateBlock extends BlockImportResult

case class ChainReorganised(oldBranch: List[Block], newBranch: List[Block], totalDifficulties: List[BigInt]) extends BlockImportResult

case class BlockImportFailed(error: String) extends BlockImportResult

case object UnknownParent extends BlockImportResult
