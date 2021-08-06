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
  sealed trait ConsensusResult

  case class ExtendedCurrentBestBranch(blockImportData: List[BlockData]) extends ConsensusResult
  case class ExtendedCurrentBestBranchPartially(blockImportData: List[BlockData], failureBranch: BranchExecutionFailure)
      extends ConsensusResult

  case class SelectedNewBestBranch(oldBranch: List[Block], newBranch: List[Block], weights: List[ChainWeight])
      extends ConsensusResult

  case object KeptCurrentBestBranch extends ConsensusResult

  case class BranchExecutionFailure(failingBlockHash: ByteString, error: String) extends ConsensusResult

  case class ConsensusError(err: String) extends ConsensusResult
  case class ConsensusErrorDueToMissingNode(reason: MissingNodeException) extends ConsensusResult
}
