package io.iohk.ethereum.consensus

import akka.util.ByteString

import cats.implicits._

import monix.eval.Task
import monix.execution.Scheduler

import scala.annotation.tailrec

import org.bouncycastle.util.encoders.Hex

import io.iohk.ethereum.blockchain.sync.regular.BlockEnqueued
import io.iohk.ethereum.blockchain.sync.regular.BlockImportFailed
import io.iohk.ethereum.blockchain.sync.regular.BlockImportFailedDueToMissingNode
import io.iohk.ethereum.blockchain.sync.regular.BlockImportResult
import io.iohk.ethereum.blockchain.sync.regular.BlockImportedToTop
import io.iohk.ethereum.blockchain.sync.regular.ChainReorganised
import io.iohk.ethereum.blockchain.sync.regular.DuplicateBlock
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.BlockchainWriter
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.ledger.BlockData
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockExecutionError
import io.iohk.ethereum.ledger.BlockExecutionError.MPTError
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.BlockExecutionSuccess
import io.iohk.ethereum.ledger.BlockMetrics
import io.iohk.ethereum.ledger.BlockQueue
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.FunctorOps._
import io.iohk.ethereum.utils.Logger

class ConsensusImpl(
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    blockchainWriter: BlockchainWriter,
    blockQueue: BlockQueue,
    blockValidation: BlockValidation,
    blockExecution: BlockExecution,
    validationScheduler: Scheduler // Can't be implicit because of importToTop method and ambiguous of Scheduler
) extends Consensus
    with Logger {

  /** Tries to import the block as the new best block in the chain or enqueue it for later processing.
    *
    * @param block                   block to be imported
    * @param blockExecutionScheduler threadPool on which the execution should be run
    * @param blockchainConfig        blockchain configuration
    * @return One of:
    *   - [[BlockImportedToTop]] - if the block was added as the new best block
    *   - [[BlockEnqueued]]      - block is stored in the [[io.iohk.ethereum.ledger.BlockQueue]]
    *   - [[ChainReorganised]]   - a better new branch was found causing chain reorganisation
    *   - [[DuplicateBlock]]     - block already exists either in the main chain or in the queue
    *   - [[BlockImportFailed]]  - block failed to execute (when importing to top or reorganising the chain)
    */
  override def evaluateBranchBlock(
      block: Block
  )(implicit blockExecutionScheduler: Scheduler, blockchainConfig: BlockchainConfig): Task[BlockImportResult] =
    blockchainReader.getBestBlock() match {
      case Some(bestBlock) =>
        if (isBlockADuplicate(block.header, bestBlock.header.number)) {
          log.debug("Ignoring duplicated block: {}", block.idTag)
          Task.now(DuplicateBlock)
        } else {
          blockchain.getChainWeightByHash(bestBlock.header.hash) match {
            case Some(weight) =>
              doBlockPreValidation(block).flatMap {
                case Left(error) => Task.now(BlockImportFailed(error.reason.toString))
                case Right(_)    => handleBlockImport(block, bestBlock, weight)
              }
            case None => returnNoTotalDifficulty(bestBlock)
          }
        }
      case None => returnNoBestBlock()
    }

  private def handleBlockImport(block: Block, bestBlock: Block, weight: ChainWeight)(implicit
      blockExecutionScheduler: Scheduler,
      blockchainConfig: BlockchainConfig
  ): Task[BlockImportResult] = {
    val importResult = if (isPossibleNewBestBlock(block.header, bestBlock.header)) {
      importToTop(block, bestBlock, weight)
    } else {
      reorganiseOrEnqueue(block, bestBlock, weight)
    }
    importResult.foreach(measureBlockMetrics)
    importResult
  }

  private def doBlockPreValidation(block: Block)(implicit
      blockchainConfig: BlockchainConfig
  ): Task[Either[ValidationBeforeExecError, BlockExecutionSuccess]] =
    Task
      .evalOnce(blockValidation.validateBlockBeforeExecution(block))
      .tap {
        case Left(error) =>
          log.error("Error while validating block with hash {} before execution: {}", block.hash, error.reason)
        case Right(_) => log.debug("Block with hash {} validated successfully", block.hash)
      }
      .executeOn(validationScheduler)

  private def returnNoTotalDifficulty(bestBlock: Block): Task[BlockImportFailed] = {
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

  private def returnNoBestBlock(): Task[BlockImportFailed] = {
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
    newBlock.number == currentBestBlock.number + 1 && newBlock.parentHash == currentBestBlock.hash

  private def measureBlockMetrics(importResult: BlockImportResult): Unit =
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
  )(implicit blockExecutionScheduler: Scheduler, blockchainConfig: BlockchainConfig): Task[BlockImportResult] =
    Task
      .evalOnce(importBlockToTop(block, currentBestBlock.header.number, currentWeight))
      .executeOn(blockExecutionScheduler)

  /** *
    * Open for discussion: this is code that was in BlockImport. Even thought is tested (before) that only one block
    * is being added to the blockchain, this code assumes that the import may be of more than one block (a branch)
    * and because it is assuming that it may be dealing with a branch the code to handle errors assumes several scenarios.
    * Is there is reason for this over-complication?
    */
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
          case None => BlockImportedToTop(importedBlocks)

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

        importedBlocks.foreach { blockData =>
          val header = blockData.block.header
          log.debug(
            "Imported new block ({}: {}) to the top of chain \n",
            header.number,
            Hex.toHexString(header.hash.toArray)
          )
        }

        result

      case None =>
        BlockImportFailed(s"Newly enqueued block with hash: ${block.header.hash} is not part of a known branch")
    }
  }

  private def reorganiseOrEnqueue(
      block: Block,
      currentBestBlock: Block,
      currentWeight: ChainWeight
  )(implicit blockchainConfig: BlockchainConfig): Task[BlockImportResult] =
    Task.evalOnce(blockQueue.enqueueBlock(block, currentBestBlock.header.number) match {
      case Some(Leaf(leafHash, leafWeight)) if leafWeight > currentWeight =>
        reorganiseChainFromQueue(leafHash)

      case _ =>
        BlockEnqueued
    })

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
    log.info("Reorganising chain from leaf {}", ByteStringUtils.hash2string(queuedLeaf))
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
      blockchainReader.getBlockByNumber(blockchainReader.getBestBranch(), fromNumber) match {
        case Some(block) if block.header.hash == parent || fromNumber == 0 =>
          acc

        case Some(block) =>
          val hash = block.header.hash

          val blockDataOpt = for {
            receipts <- blockchainReader.getReceiptsByHash(hash)
            weight <- blockchain.getChainWeightByHash(hash)
          } yield BlockData(block, receipts, weight)

          blockchain.removeBlock(hash)

          removeBlocksUntil(parent, fromNumber - 1, blockDataOpt.map(_ :: acc).getOrElse(acc))

        case None =>
          log.error(s"Unexpected missing block number: $fromNumber")
          acc
      }

    removeBlocksUntil(parent, fromNumber, Nil)
  }
}
