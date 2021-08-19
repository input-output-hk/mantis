package io.iohk.ethereum.consensus

import akka.util.ByteString

import cats.data.NonEmptyList
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
import io.iohk.ethereum.consensus.Consensus._
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.BlockchainWriter
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.ledger.BlockData
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockExecutionError
import io.iohk.ethereum.ledger.BlockExecutionError.MPTError
import io.iohk.ethereum.ledger.BlockMetrics
import io.iohk.ethereum.ledger.BlockQueue
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Logger

class ConsensusImpl(
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    blockchainWriter: BlockchainWriter,
    blockExecution: BlockExecution
) extends Consensus
    with Logger {

  /** Try to set the given branch as the new best branch if it is better than the current best
    * branch.
    * @param branch                  the new branch as a sorted list of blocks. It's parent must
    *                                be in the current best branch
    * @param blockExecutionScheduler threadPool on which the execution should be run
    * @param blockchainConfig        blockchain configuration
    * @return One of:
    *   - [[ExtendedCurrentBestBranch]] - if the branch was added on top of the current branch
    *   - [[SelectedNewBestBranch]]     - if the chain was reorganized.
    *   - [[KeptCurrentBestBranch]]     - if the branch was not considered as better than the current branch
    *   - [[ConsensusError]]            - block failed to execute (when importing to top or reorganising the chain)
    *   - [[ConsensusErrorDueToMissingNode]]  - block failed to execute (when importing to top or reorganising the chain)
    */
  override def evaluateBranch(
      branch: NonEmptyList[Block]
  )(implicit blockExecutionScheduler: Scheduler, blockchainConfig: BlockchainConfig): Task[ConsensusResult] =
    blockchainReader.getBestBlock() match {
      case Some(bestBlock) =>
        blockchainReader.getChainWeightByHash(bestBlock.header.hash) match {
          case Some(weight) => handleBranchImport(branch, bestBlock, weight)
          case None         => returnNoTotalDifficulty(bestBlock)
        }
      case None => returnNoBestBlock()
    }

  private def handleBranchImport(
      branch: NonEmptyList[Block],
      currentBestBlock: Block,
      currentBestBlockWeight: ChainWeight
  )(implicit
      blockExecutionScheduler: Scheduler,
      blockchainConfig: BlockchainConfig
  ): Task[ConsensusResult] = {
    val parentHash = branch.head.header.parentHash
    val parentWeight = blockchainReader
      .getChainWeightByHash(parentHash)
      .getOrElse(throw new Error("Inconsistent database"))

    val consensusResult: Task[ConsensusResult] =
      if (newBranchWeight(branch, parentWeight) > currentBestBlockWeight) {
        if (currentBestBlock.isParentOf(branch.head)) {
          Task.evalOnce(importToTop(branch, currentBestBlockWeight)).executeOn(blockExecutionScheduler)
        } else {
          Task.evalOnce(reorganise(branch, parentWeight, parentHash)).executeOn(blockExecutionScheduler)
        }
      } else {
        Task.now(KeptCurrentBestBranch)
      }

    consensusResult.foreach(measureBlockMetrics)
    consensusResult
  }

  private def importToTop(branch: NonEmptyList[Block], currentBestBlockWeight: ChainWeight)(implicit
      blockExecutionScheduler: Scheduler,
      blockchainConfig: BlockchainConfig
  ): ConsensusResult =
    blockExecution.executeAndValidateBlocks(branch.toList, currentBestBlockWeight) match {
      case (importedBlocks, None) =>
        ExtendedCurrentBestBranch(importedBlocks)
      case (_, Some(MPTError(reason))) if reason.isInstanceOf[MissingNodeException] =>
        ConsensusErrorDueToMissingNode(Nil, reason.asInstanceOf[MissingNodeException])
      case (Nil, Some(error)) =>
        BranchExecutionFailure(Nil, branch.head.header.hash, error.toString)
      case (importedBlocks, Some(error)) =>
        val failingBlock = branch.toList.drop(importedBlocks.length).head
        ExtendedCurrentBestBranchPartially(
          importedBlocks,
          BranchExecutionFailure(Nil, failingBlock.hash, error.toString)
        )
    }

  private def reorganise(newBranch: NonEmptyList[Block], parentWeight: ChainWeight, parentHash: ByteString)(implicit
      blockchainConfig: BlockchainConfig
  ): ConsensusResult = {

    val bestNumber = blockchainReader.getBestBlockNumber()
    log.debug(
      "Removing blocks starting from number {} and parent {}",
      bestNumber,
      ByteStringUtils.hash2string(parentHash)
    )
    val oldBlocksData = removeBlocksUntil(parentHash, bestNumber)

    handleBlockExecResult(newBranch.toList, parentWeight, oldBlocksData).fold(
      {
        case (executedBlocks, MPTError(reason: MissingNodeException)) =>
          ConsensusErrorDueToMissingNode(executedBlocks.map(_.block), reason)
        case (executedBlocks, err) =>
          BranchExecutionFailure(
            executedBlocks.map(_.block),
            newBranch.toList.drop(executedBlocks.length).head.hash,
            s"Error while trying to reorganise chain: $err"
          )
      },
      SelectedNewBestBranch.tupled
    )
  }

  private def newBranchWeight(newBranch: NonEmptyList[Block], parentWeight: ChainWeight) =
    newBranch.foldLeft(parentWeight)((w, b) => w.increase(b.header))

  private def returnNoTotalDifficulty(bestBlock: Block): Task[ConsensusError] = {
    log.error(
      "Getting total difficulty for current best block with hash: {} failed",
      bestBlock.header.hashAsHexString
    )
    Task.now(
      ConsensusError(
        Nil,
        s"Couldn't get total difficulty for current best block with hash: ${bestBlock.header.hashAsHexString}"
      )
    )
  }

  private def returnNoBestBlock(): Task[ConsensusError] = {
    log.error("Getting current best block failed")
    Task.now(ConsensusError(Nil, "Couldn't find the current best block"))
  }

  private def measureBlockMetrics(importResult: ConsensusResult): Unit =
    importResult match {
      case ExtendedCurrentBestBranch(blockImportData) =>
        blockImportData.foreach(blockData => BlockMetrics.measure(blockData.block, blockchainReader.getBlockByHash))
      case SelectedNewBestBranch(_, newBranch, _) =>
        newBranch.foreach(block => BlockMetrics.measure(block, blockchainReader.getBlockByHash))
      case _ => ()
    }

  private def handleBlockExecResult(
      newBranch: List[Block],
      parentWeight: ChainWeight,
      oldBlocksData: List[BlockData]
  )(implicit
      blockchainConfig: BlockchainConfig
  ): Either[(List[BlockData], BlockExecutionError), (List[Block], List[Block], List[ChainWeight])] = {
    val (executedBlocks, maybeError) = blockExecution.executeAndValidateBlocks(newBranch, parentWeight)
    maybeError match {
      case None =>
        Right((oldBlocksData.map(_.block), executedBlocks.map(_.block), executedBlocks.map(_.weight)))

      case Some(error) =>
        revertChainReorganisation(newBranch, oldBlocksData, executedBlocks)
        Left((executedBlocks, error))
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

    val bestHeader = oldBranch.last.block.header
    blockchain.saveBestKnownBlocks(bestHeader.hash, bestHeader.number, checkpointNumber)

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
            weight <- blockchainReader.getChainWeightByHash(hash)
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
