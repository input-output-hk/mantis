package io.iohk.ethereum.consensus

import akka.util.ByteString

import cats.data.NonEmptyList

import monix.eval.Task
import monix.execution.Scheduler

import io.iohk.ethereum.consensus.Consensus.ConsensusResult
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.ledger.BlockData
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.utils.BlockchainConfig

/** This file documents the original interface that was designed at ETCM-1018
  * but implements a different one to be used as a stepping stone to the new architecture
  * still in progress
  */
trait Consensus {
  def evaluateBranch(
      block: NonEmptyList[Block]
  )(implicit blockExecutionScheduler: Scheduler, blockchainConfig: BlockchainConfig): Task[ConsensusResult]

  /** Original interface from ETCM-1018, for temporary documentation purposes
    */
  /** Answer which branch is best
    * @return branch.Branch
    */
//  def getBestBranch(): branch.Branch = blockchainReader.getBestBranch()

  /** @param branch
    * This methods received a Branch that was updated by ChainManagement.
    * When a Branch is updated we need to compare the weight of the current best branch with the
    * updated one.
    * If the current best branch is still the best then nothing needs to be done.
    * If the updated branch is heavier than an attempt to set the updated branch as best branch is done by
    * executing the blocks in the updated branch to see if it is a valid branch.
    * If it is not a valid branch then ExecutingSync has to be informed, otherwise update state with new best branch.
    */
//  def evaluateBranch(branch: UpdatedBranch): Either[BlockExecutionError, Boolean] =
//    if (extendsBestBranch()) {
//      // just validate the latest block
//      Right(true)
//    } else {
//      if (isHeavierThanBestBranch(branch)) {
//        // create a queue of (branchTip, CancelableFuture)
//        // if any branch is being executed at the moment while a better one comes is then call the cancellation hook
//        attemptToSetNewBestBranch(branch) match {
//          case Right(result) => // save pointer to new best branch
//            Right(true)
//          case Left(error) => Left(error)
//        }
//      } else {
//        // nothing
//        Right(true)
//      }
//    }

//  private def extendsBestBranch(): Boolean = ???

  /** Compares the weight of the updatedBranch with the weight of the current best branch
    * @param updatedBranch
    * @return true if updatedBranch is heavier than current best branch, false otherwise
    */
//  private def isHeavierThanBestBranch(updatedBranch: UpdatedBranch): Boolean = ???

  /** Tries to set a new best branch by executing all blocks in the branch, from the HCB to the branch tip.
    * We assume the pre validation of the blocks of the branch was done already
    * @param branch
    * @return  Either[BlockExecutionError, Boolean]
    */
//  private def attemptToSetNewBestBranch(branch: UpdatedBranch): Either[BlockExecutionError, Boolean] = ???

}

object Consensus {
  /* This return type for consensus is probably overcomplicated for now because some information is needed
   * to keep the compatibility with the current code (particularly for the block queue handling), and be able
   * to translate the values to BlockImportResult.
   * In particular:
   *  - `blockToEnqueue` fields won't be needed if the block are already stored in memory
   *  - The distinction between ExtendedCurrentBestBranch and SelectedNewBestBranch won't really be useful
   *  because there will be no need to put back the old branch into the block queue in case of reorganisation
   *  - `ConsensusErrorDueToMissingNode` and  `ConsensusError` would mean that the application is in an
   *  inconsistent state. Unless there is a reason to think that mantis would self heal when that happens, I
   *  don't think there is a reason to add them here.
   */

  sealed trait ConsensusResult

  /** The new branch was selected and it extended the best branch. */
  case class ExtendedCurrentBestBranch(blockImportData: List[BlockData]) extends ConsensusResult

  /** The new branch was selected and it extended the best branch, but it did not execute completely. */
  case class ExtendedCurrentBestBranchPartially(blockImportData: List[BlockData], failureBranch: BranchExecutionFailure)
      extends ConsensusResult

  /** The new branch was selected but was not an extension of the best branch. */
  case class SelectedNewBestBranch(oldBranch: List[Block], newBranch: List[Block], weights: List[ChainWeight])
      extends ConsensusResult

  /** The proposed new branch was not better than the current best one. */
  case object KeptCurrentBestBranch extends ConsensusResult

  /** A block in the branch cannot be executed. */
  case class BranchExecutionFailure(blockToEnqueue: List[Block], failingBlockHash: ByteString, error: String)
      extends ConsensusResult

  /** An error external the the blocks in the branch occured, which prevents the branch from being executed.
    * Usually this is due to an inconsistency in the database.
    */
  case class ConsensusError(blockToEnqueue: List[Block], err: String) extends ConsensusResult
  case class ConsensusErrorDueToMissingNode(blockToEnqueue: List[Block], reason: MissingNodeException)
      extends ConsensusResult
}
