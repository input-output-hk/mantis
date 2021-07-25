package io.iohk.ethereum.ledger

import akka.util.ByteString

import monix.eval.Task
import monix.execution.Scheduler

import scala.annotation.tailrec

import org.bouncycastle.util.encoders.Hex

import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.MPTError
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Logger

class BlockImport(
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    blockchainWriter: BlockchainWriter,
    blockQueue: BlockQueue,
    blockValidation: BlockValidation,
    private[ledger] val blockExecution: BlockExecution,
    validationScheduler: Scheduler // Can't be implicit because of importToTop method and ambiguous of Scheduler
) extends Logger {

  /** Tries to import the block as the new best block in the chain or enqueue it for later processing.
    *
    * @param block                 block to be imported
    * @param blockExecutionContext threadPool on which the execution should be run
    * @return One of:
    *         - [[io.iohk.ethereum.ledger.BlockImportedToTop]] - if the block was added as the new best block
    *         - [[io.iohk.ethereum.ledger.BlockEnqueued]] - block is stored in the [[io.iohk.ethereum.ledger.BlockQueue]]
    *         - [[io.iohk.ethereum.ledger.ChainReorganised]] - a better new branch was found causing chain reorganisation
    *         - [[io.iohk.ethereum.ledger.DuplicateBlock]] - block already exists either in the main chain or in the queue
    *         - [[io.iohk.ethereum.ledger.BlockImportFailed]] - block failed to execute (when importing to top or reorganising the chain)
    */
  def importBlock(
      block: Block
  )(implicit blockExecutionScheduler: Scheduler, blockchainConfig: BlockchainConfig): Task[BlockImportResult] =
    blockchainReader.getBestBlock() match {
      case Some(bestBlock) =>
        if (isBlockADuplicate(block.header, bestBlock.header.number)) {
          Task(log.debug(s"Ignoring duplicate block: (${block.idTag})"))
            .map(_ => DuplicateBlock)
        } else {
          val hash = bestBlock.header.hash
          blockchain.getChainWeightByHash(hash) match {
            case Some(weight) =>
              val importResult = if (isPossibleNewBestBlock(block.header, bestBlock.header)) {
                importToTop(block, bestBlock, weight)
              } else {
                reorganise(block, bestBlock, weight)
              }
              importResult.foreach(measureBlockMetrics)
              importResult
            case None =>
              log.error(
                "Getting total difficulty for current best block with hash: {} failed",
                bestBlock.header.hashAsHexString
              )
              Task.now(
                BlockImportFailed(
                  s"Couldn't get total difficulty for current best block with hash: ${bestBlock.header.hashAsHexString}"
                )
              )
          }
        }
      case None =>
        log.error("Getting current best block failed")
        Task.now(BlockImportFailed("Couldn't find the current best block"))
    }

  private def isBlockADuplicate(block: BlockHeader, currentBestBlockNumber: BigInt): Boolean = {
    val hash = block.hash
    blockchainReader.getBlockByHash(hash).isDefined && block.number <= currentBestBlockNumber || blockQueue.isQueued(
      hash
    )
  }

  private def isPossibleNewBestBlock(newBlock: BlockHeader, currentBestBlock: BlockHeader): Boolean =
    newBlock.parentHash == currentBestBlock.hash && newBlock.number == currentBestBlock.number + 1

  private def measureBlockMetrics(importResult: BlockImportResult)(implicit blockchainConfig: BlockchainConfig): Unit =
    importResult match {
      case BlockImportedToTop(blockImportData) =>
        blockImportData.foreach(blockData => BlockMetrics.measure(blockData.block, blockchainReader.getBlockByHash))
      case ChainReorganised(_, newBranch, _) =>
        newBranch.foreach(block => BlockMetrics.measure(block, blockchainReader.getBlockByHash))
      case _ => ()
    }

  private def importToTop(
      block: Block,
      currentBestBlock: Block,
      currentWeight: ChainWeight
  )(implicit blockExecutionScheduler: Scheduler, blockchainConfig: BlockchainConfig): Task[BlockImportResult] = {
    val validationResult =
      Task.evalOnce(blockValidation.validateBlockBeforeExecution(block)).executeOn(validationScheduler)
    val importResult =
      Task
        .evalOnce(importBlockToTop(block, currentBestBlock.header.number, currentWeight))
        .executeOn(blockExecutionScheduler)

    Task.parMap2(validationResult, importResult) { case (validationResult, importResult) =>
      validationResult.fold(
        error => {
          log.error("Error while validation block before execution: {}", error.reason)
          handleImportTopValidationError(error, block, importResult)
        },
        _ => importResult
      )
    }
  }

  private def importBlockToTop(block: Block, bestBlockNumber: BigInt, currentWeight: ChainWeight)(implicit
      blockchainConfig: BlockchainConfig
  ): BlockImportResult = {
    val executionResult = for {
      topBlock <- blockQueue.enqueueBlock(block, bestBlockNumber)
      topBlocks = blockQueue.getBranch(topBlock.hash, dequeue = true)
      (executed, errors) = blockExecution.executeAndValidateBlocks(topBlocks, currentWeight)
    } yield (executed, errors, topBlocks)

    executionResult match {
      case Some((importedBlocks, maybeError, topBlocks)) =>
        val result = maybeError match {
          case None =>
            BlockImportedToTop(importedBlocks)

          case Some(MPTError(reason)) if reason.isInstanceOf[MissingNodeException] =>
            BlockImportFailedDueToMissingNode(reason.asInstanceOf[MissingNodeException])

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

  private def reorganise(
      block: Block,
      currentBestBlock: Block,
      currentWeight: ChainWeight
  )(implicit blockchainConfig: BlockchainConfig): Task[BlockImportResult] =
    Task.evalOnce {
      blockValidation
        .validateBlockBeforeExecution(block)
        .fold(
          error => handleBlockValidationError(error, block),
          _ =>
            blockQueue.enqueueBlock(block, currentBestBlock.header.number) match {
              case Some(Leaf(leafHash, leafWeight)) if leafWeight > currentWeight =>
                log.debug("Found a better chain, about to reorganise")
                reorganiseChainFromQueue(leafHash)

              case _ =>
                BlockEnqueued
            }
        )
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
  )(implicit blockchainConfig: BlockchainConfig): BlockImportResult = {
    log.debug("Reorganising chain from leaf {}", ByteStringUtils.hash2string(queuedLeaf))
    val newBranch = blockQueue.getBranch(queuedLeaf, dequeue = true)
    val bestNumber = blockchainReader.getBestBlockNumber()

    val reorgResult = for {
      parent <- newBranch.headOption
      parentHash = parent.header.parentHash
      parentWeight <- blockchain.getChainWeightByHash(parentHash)
    } yield {
      log.debug(
        "Removing blocks starting from number {} and parent {}",
        bestNumber,
        ByteStringUtils.hash2string(parentHash)
      )
      val oldBlocksData = removeBlocksUntil(parentHash, bestNumber)
      oldBlocksData.foreach(block => blockQueue.enqueueBlock(block.block))
      handleBlockExecResult(newBranch, parentWeight, oldBlocksData)
    }

    reorgResult match {
      case Some(execResult) =>
        execResult.fold(
          {
            case MPTError(reason: MissingNodeException) => BlockImportFailedDueToMissingNode(reason)
            case err                                    => BlockImportFailed(s"Error while trying to reorganise chain: $err")
          },
          ChainReorganised.tupled
        )

      case None =>
        BlockImportFailed("Error while trying to reorganise chain with parent of new branch")
    }
  }

  private def handleBlockExecResult(
      newBranch: List[Block],
      parentWeight: ChainWeight,
      oldBlocksData: List[BlockData]
  )(implicit
      blockchainConfig: BlockchainConfig
  ): Either[BlockExecutionError, (List[Block], List[Block], List[ChainWeight])] = {
    val (executedBlocks, maybeError) = blockExecution.executeAndValidateBlocks(newBranch, parentWeight)
    maybeError match {
      case None =>
        Right((oldBlocksData.map(_.block), executedBlocks.map(_.block), executedBlocks.map(_.weight)))

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

    oldBranch.foreach { case BlockData(block, receipts, weight) =>
      blockchainWriter.save(block, receipts, weight, saveAsBestBlock = false)
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
    @tailrec
    def removeBlocksUntil(parent: ByteString, fromNumber: BigInt, acc: List[BlockData]): List[BlockData] =
      blockchainReader.getBestBranch().getBlockByNumber(fromNumber) match {
        case Some(block) if block.header.hash == parent || fromNumber == 0 =>
          acc

        case Some(block) =>
          val hash = block.header.hash

          val blockDataOpt = for {
            receipts <- blockchainReader.getReceiptsByHash(hash)
            weight <- blockchain.getChainWeightByHash(hash)
          } yield BlockData(block, receipts, weight)

          blockchain.removeBlock(hash, withState = true)

          removeBlocksUntil(parent, fromNumber - 1, blockDataOpt.map(_ :: acc).getOrElse(acc))

        case None =>
          log.error(s"Unexpected missing block number: $fromNumber")
          acc
      }

    removeBlocksUntil(parent, fromNumber, Nil)
  }
}

sealed trait BlockImportResult

case class BlockImportedToTop(blockImportData: List[BlockData]) extends BlockImportResult

case object BlockEnqueued extends BlockImportResult

case object DuplicateBlock extends BlockImportResult

case class ChainReorganised(
    oldBranch: List[Block],
    newBranch: List[Block],
    weights: List[ChainWeight]
) extends BlockImportResult

case class BlockImportFailed(error: String) extends BlockImportResult

case class BlockImportFailedDueToMissingNode(reason: MissingNodeException) extends BlockImportResult

case object UnknownParent extends BlockImportResult
