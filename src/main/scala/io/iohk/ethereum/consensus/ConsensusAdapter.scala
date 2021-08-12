package io.iohk.ethereum.consensus

import cats.data.NonEmptyList

import monix.eval.Task
import monix.execution.Scheduler

import io.iohk.ethereum.blockchain.sync.regular.BlockEnqueued
import io.iohk.ethereum.blockchain.sync.regular.BlockImportFailed
import io.iohk.ethereum.blockchain.sync.regular.BlockImportFailedDueToMissingNode
import io.iohk.ethereum.blockchain.sync.regular.BlockImportResult
import io.iohk.ethereum.blockchain.sync.regular.BlockImportedToTop
import io.iohk.ethereum.blockchain.sync.regular.ChainReorganised
import io.iohk.ethereum.blockchain.sync.regular.DuplicateBlock
import io.iohk.ethereum.consensus.Consensus.BranchExecutionFailure
import io.iohk.ethereum.consensus.Consensus.ConsensusError
import io.iohk.ethereum.consensus.Consensus.ConsensusErrorDueToMissingNode
import io.iohk.ethereum.consensus.Consensus.ExtendedCurrentBestBranch
import io.iohk.ethereum.consensus.Consensus.ExtendedCurrentBestBranchPartially
import io.iohk.ethereum.consensus.Consensus.KeptCurrentBestBranch
import io.iohk.ethereum.consensus.Consensus.SelectedNewBestBranch
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.BlockExecutionSuccess
import io.iohk.ethereum.ledger.BlockQueue
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.FunctorOps._
import io.iohk.ethereum.utils.Hex
import io.iohk.ethereum.utils.Logger

/** This is a temporary class to isolate the real Consensus and extract responsibilities which should not
  * be part of the consensus in the final design, but are currently needed.
  */
class ConsensusAdapter(
    consensus: Consensus,
    blockchainReader: BlockchainReader,
    blockchain: Blockchain,
    blockQueue: BlockQueue,
    blockValidation: BlockValidation,
    validationScheduler: Scheduler
) extends Logger {
  def evaluateBranchBlock(
      block: Block
  )(implicit blockExecutionScheduler: Scheduler, blockchainConfig: BlockchainConfig): Task[BlockImportResult] =
    blockchainReader.getBestBlock() match {
      case Some(bestBlock) =>
        if (isBlockADuplicate(block.header, bestBlock.header.number)) {
          log.debug("Ignoring duplicated block: {}", block.idTag)
          Task.now(DuplicateBlock)
        } else if (blockchainReader.getChainWeightByHash(bestBlock.header.hash).isEmpty) {
          // This part is not really needed except for compatibility as a missing chain weight
          // would indicate an inconsistent database
          returnNoTotalDifficulty(bestBlock)
        } else {
          doBlockPreValidation(block).flatMap {
            case Left(error) =>
              Task.now(BlockImportFailed(error.reason.toString))
            case Right(BlockExecutionSuccess) =>
              enqueueAndGetBranch(block, bestBlock.number)
                .map(forwardAndTranslateConsensusResult) // a new branch was created so we give it to consensus
                .getOrElse(Task.now(BlockEnqueued)) // the block was not rooted so it was simply enqueued
          }
        }
      case None =>
        log.error("Couldn't find the current best block")
        Task.now(BlockImportFailed("Couldn't find the current best block"))
    }

  private def forwardAndTranslateConsensusResult(
      newBranch: NonEmptyList[Block]
  )(implicit blockExecutionScheduler: Scheduler, blockchainConfig: BlockchainConfig) =
    consensus
      .evaluateBranch(newBranch)
      .map {
        case SelectedNewBestBranch(oldBranch, newBranch, weights) =>
          oldBranch.foreach(blockQueue.enqueueBlock(_))
          ChainReorganised(oldBranch, newBranch, weights)
        case ExtendedCurrentBestBranch(blockImportData) =>
          BlockImportedToTop(blockImportData)
        case ExtendedCurrentBestBranchPartially(
              blockImportData,
              BranchExecutionFailure(blocksToEnqueue, failingBlockHash, error)
            ) =>
          blocksToEnqueue.foreach(blockQueue.enqueueBlock(_))
          blockQueue.removeSubtree(failingBlockHash)
          log.warn("extended best branch partially because of error: {}", error)
          BlockImportedToTop(blockImportData)
        case KeptCurrentBestBranch =>
          newBranch.toList.foreach(blockQueue.enqueueBlock(_))
          BlockEnqueued
        case BranchExecutionFailure(blocksToEnqueue, failingBlockHash, error) =>
          blocksToEnqueue.foreach(blockQueue.enqueueBlock(_))
          blockQueue.removeSubtree(failingBlockHash)
          BlockImportFailed(error)
        case ConsensusError(blocksToEnqueue, error) =>
          blocksToEnqueue.foreach(blockQueue.enqueueBlock(_))
          BlockImportFailed(error)
        case ConsensusErrorDueToMissingNode(blocksToEnqueue, reason) =>
          blocksToEnqueue.foreach(blockQueue.enqueueBlock(_))
          BlockImportFailedDueToMissingNode(reason)
      }

  private def doBlockPreValidation(block: Block)(implicit
      blockchainConfig: BlockchainConfig
  ): Task[Either[ValidationBeforeExecError, BlockExecutionSuccess]] =
    Task
      .evalOnce(blockValidation.validateBlockBeforeExecution(block))
      .tap {
        case Left(error) =>
          log.error(
            "Error while validating block with hash {} before execution: {}",
            Hex.toHexString(block.hash.toArray),
            error.reason.toString
          )
        case Right(_) => log.debug("Block with hash {} validated successfully", Hex.toHexString(block.hash.toArray))
      }
      .executeOn(validationScheduler)

  private def isBlockADuplicate(block: BlockHeader, currentBestBlockNumber: BigInt): Boolean = {
    val hash = block.hash
    blockchainReader.getBlockByHash(hash).isDefined &&
    block.number <= currentBestBlockNumber ||
    blockQueue.isQueued(hash)
  }

  private def enqueueAndGetBranch(block: Block, bestBlockNumber: BigInt): Option[NonEmptyList[Block]] =
    blockQueue
      .enqueueBlock(block, bestBlockNumber)
      .map(topBlock => blockQueue.getBranch(topBlock.hash, dequeue = true))
      .flatMap(NonEmptyList.fromList)

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
}
