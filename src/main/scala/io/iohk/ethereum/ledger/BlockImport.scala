package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{UnKnownExecutionError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.{ExecutionContext, Future}

class BlockImport(
    blockchain: BlockchainImpl,
    blockQueue: BlockQueue,
    blockchainConfig: BlockchainConfig,
    blockValidation: BlockValidation,
    blockExecution: BlockExecution,
    validationContext: ExecutionContext // Can't be implicit because of importToTop method and ambiguous of executionContext
) extends Logger {

  private[ledger] def importToTop(
      block: Block,
      currentBestBlock: Block,
      currentTd: BigInt
  )(implicit blockExecutionContext: ExecutionContext): Future[BlockImportResult] = {
    val validationResult = Future(blockValidation.validateBlockBeforeExecution(block))(validationContext)
    val importResult = Future(importBlockToTop(block, currentBestBlock.header.number, currentTd))(blockExecutionContext)

    for {
      validationResult <- validationResult
      importResult <- importResult
    } yield {
      validationResult.fold(
        error => handleImportTopValidationError(error, block, importResult),
        _ => importResult
      )
    }
  }

  private def importBlockToTop(block: Block, bestBlockNumber: BigInt, currentTd: BigInt): BlockImportResult = {
    val executionResult = for {
      topBlock <- blockQueue.enqueueBlock(block, bestBlockNumber)
      topBlocks = blockQueue.getBranch(topBlock.hash, dequeue = true)
      (executed, errors) = blockExecution.executeBlocks(topBlocks, currentTd)
    } yield (executed, errors, topBlocks)

    executionResult match {
      case Some((importedBlocks, maybeError, topBlocks)) =>
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
        log.debug(
          "{}", {
            val result = importedBlocks.map { blockData =>
              val header = blockData.block.header
              s"Imported new block (${header.number}: ${Hex.toHexString(header.hash.toArray)}) to the top of chain \n"
            }
            result.toString
          }
        )

        if (importedBlocks.nonEmpty) {
          importedBlocks.map(blockData => BlockMetrics.measure(blockData.block, blockchain.getBlockByHash))
        }

        result

      case None =>
        BlockImportFailed(s"Newly enqueued block with hash: ${block.header.hash} is not part of a known branch")
    }
  }

  private def handleImportTopValidationError(
      error: ValidationBeforeExecError,
      block: Block,
      blockImportResult: BlockImportResult
  ): BlockImportResult = {
    blockImportResult match {
      case BlockImportedToTop(blockImportData) =>
        blockImportData.foreach { blockData =>
          val hash = blockData.block.header.hash
          blockQueue.removeSubtree(hash)
          blockchain.removeBlock(hash, withState = true)
        }
      case _ => ()
    }
    handleBlockValidationError(error, block)
  }

  private def handleBlockValidationError(error: ValidationBeforeExecError, block: Block): BlockImportResult =
    error match {
      case ValidationBeforeExecError(HeaderParentNotFoundError) =>
        log.debug(s"Block(${block.idTag}) has unknown parent")
        UnknownParent

      case ValidationBeforeExecError(reason) =>
        log.debug(s"Block(${block.idTag}) failed pre-import validation")
        BlockImportFailed(reason.toString)
    }

  private[ledger] def reorganise(
      block: Block,
      currentBestBlock: Block,
      currentTd: BigInt
  )(implicit blockExecutionContext: ExecutionContext): Future[BlockImportResult] = Future {
    blockValidation
      .validateBlockBeforeExecution(block)
      .fold(
        error => handleBlockValidationError(error, block),
        _ => {
          blockQueue.enqueueBlock(block, currentBestBlock.header.number) match {
            case Some(Leaf(leafHash, leafTd)) if isBetterBranch(block, currentBestBlock, leafTd, currentTd) =>
              log.debug("Found a better chain, about to reorganise")
              reorganiseChain(leafHash, leafTd)

            case _ =>
              BlockEnqueued
          }
        }
      )
  }

  private def isBetterBranch(block: Block, bestBlock: Block, newTd: BigInt, currentTd: BigInt): Boolean = {
    lazy val betterTd = newTd > currentTd
    lazy val tieBreaker =
      blockchainConfig.gasTieBreaker && newTd == currentTd && block.header.gasUsed > bestBlock.header.gasUsed

    (block.hasCheckpoint, bestBlock.hasCheckpoint) match {
      case (true, true) => false
      case (false, true) => false
      case (true, false) => true
      case (false, false) => betterTd || tieBreaker
    }
  }

  private def reorganiseChain(leafHash: ByteString, leafTd: BigInt): BlockImportResult = {
    reorganiseChainFromQueue(leafHash) match {
      case Right((oldBranch, newBranch)) =>
        val totalDifficulties = newBranch.tail.foldRight(List(leafTd)) { (b, tds) =>
          (tds.head - b.header.difficulty) :: tds
        }
        ChainReorganised(oldBranch, newBranch, totalDifficulties)

      case Left(error) =>
        BlockImportFailed(s"Error while trying to reorganise chain: $error")
    }
  }

  /** Once a better branch was found this attempts to reorganise the chain
    *
    * @param queuedLeaf a block hash that determines a new branch stored in the queue (newest block from the branch)
    *
    * @return [[BlockExecutionError]] if one of the blocks in the new branch failed to execute, otherwise:
    *        (oldBranch, newBranch) as lists of blocks
    */
  private def reorganiseChainFromQueue(
      queuedLeaf: ByteString
  ): Either[BlockExecutionError, (List[Block], List[Block])] = {
    blockchain.persistCachedNodes()
    val newBranch = blockQueue.getBranch(queuedLeaf, dequeue = true)
    val bestNumber = blockchain.getBestBlockNumber()

    val result = for {
      parent <- newBranch.headOption
      parentHash = parent.header.parentHash
      parentTd <- blockchain.getTotalDifficultyByHash(parentHash)
    } yield {
      val oldBlocksData = removeBlocksUntil(parentHash, bestNumber).reverse
      oldBlocksData.foreach(block => blockQueue.enqueueBlock(block.block))
      handleBlockExecResult(newBranch, parentTd, oldBlocksData)
    }

    result.getOrElse(Left(UnKnownExecutionError("Error while trying to reorganise chain with parent of new branch")))
  }

  private def handleBlockExecResult(
      newBranch: List[Block],
      parentTd: BigInt,
      oldBlocksData: List[BlockData]
  ): Either[BlockExecutionError, (List[Block], List[Block])] = {
    val (executedBlocks, maybeError) = blockExecution.executeBlocks(newBranch, parentTd)
    maybeError match {
      case None =>
        Right(oldBlocksData.map(_.block), executedBlocks.map(_.block))

      case Some(error) =>
        revertChainReorganisation(newBranch, oldBlocksData, executedBlocks)
        Left(error)
    }
  }

  /** Reverts chain reorganisation in the event that one of the blocks from new branch fails to execute
    *
    * @param newBranch      new blocks
    * @param oldBranch      old blocks along with corresponding receipts and totalDifficulties
    * @param executedBlocks sub-sequence of new branch that was executed correctly
    */
  private def revertChainReorganisation(
      newBranch: List[Block],
      oldBranch: List[BlockData],
      executedBlocks: List[BlockData]
  ): Unit = {
    if (executedBlocks.nonEmpty) {
      removeBlocksUntil(executedBlocks.head.block.header.parentHash, executedBlocks.last.block.header.number)
    }

    oldBranch.foreach { case BlockData(block, receipts, td) =>
      blockchain.save(block, receipts, td, saveAsBestBlock = false)
    }

    import cats.implicits._
    val checkpointNumber = oldBranch.collect {
      case BlockData(block, _, _) if block.hasCheckpoint => block.number
    }.maximumOption

    val bestNumber = oldBranch.last.block.header.number
    blockchain.saveBestKnownBlocks(bestNumber, checkpointNumber)
    executedBlocks.foreach(data => blockQueue.enqueueBlock(data.block, bestNumber))

    newBranch.diff(executedBlocks.map(_.block)).headOption.foreach { block =>
      blockQueue.removeSubtree(block.header.hash)
    }
  }

  /** Removes blocks from the [[Blockchain]] along with receipts and total difficulties.
    *
    * @param parent     remove blocks until this hash (exclusive)
    * @param fromNumber start removing from this number (downwards)
    *
    * @return the list of removed blocks along with receipts and total difficulties
    */
  private def removeBlocksUntil(parent: ByteString, fromNumber: BigInt): List[BlockData] = {
    blockchain.getBlockByNumber(fromNumber) match {
      case Some(block) if block.header.hash == parent || fromNumber == 0 =>
        Nil

      case Some(block) =>
        val hash = block.header.hash

        val blockList = for {
          receipts <- blockchain.getReceiptsByHash(hash)
          td <- blockchain.getTotalDifficultyByHash(hash)
        } yield BlockData(block, receipts, td) :: removeBlocksUntil(parent, fromNumber - 1)

        // Not updating best block number for efficiency, it will be updated in the callers anyway
        blockchain.removeBlock(hash, withState = true)

        blockList.getOrElse(Nil)

      case None =>
        log.error(s"Unexpected missing block number: $fromNumber")
        Nil
    }
  }
}

sealed trait BlockImportResult

case class BlockImportedToTop(blockImportData: List[BlockData]) extends BlockImportResult

case object BlockEnqueued extends BlockImportResult

case object DuplicateBlock extends BlockImportResult

case class ChainReorganised(
    oldBranch: List[Block],
    newBranch: List[Block],
    totalDifficulties: List[BigInt]
) extends BlockImportResult

case class BlockImportFailed(error: String) extends BlockImportResult

case object UnknownParent extends BlockImportResult
