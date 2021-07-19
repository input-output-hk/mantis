package io.iohk.ethereum.consensus

import akka.util.ByteString
import io.iohk.ethereum.domain.{Blockchain, BlockchainReader, branch}
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockImport}

// TODO do we want metrics? How often an attempt to switch to a new branch fails?

case class UpdatedBranch(tip: ByteString, weight: BigInt)

// For now we work only with the canonical (best) branch and later add the possibility of working with any branch
class Consensus(
    blockchain: Blockchain,
    blockchainReader: BlockchainReader,
    blockImport: BlockImport
) {

  /** Answer which branch is best
    * @return branch.Branch
    */
  def getBestBranch(): branch.Branch = blockchainReader.getBestBranch()

  /** @param branch
    * This methods received a Branch that was updated by ChainManagement.
    * When a Branch is updated we need to compare the weight of the current best branch with the
    * updated one.
    * If the current best branch is still the best then nothing needs to be done.
    * If the updated branch is heavier than an attempt to set the updated branch as best branch is done by
    * executing the blocks in the updated branch to see if it is a valid branch.
    * If it is not a valid branch then ExecutingSync has to be informed, otherwise update state with new best branch.
    */
  def evaluateBranch(branch: UpdatedBranch): Either[BlockExecutionError, Boolean] =
    if (extendsBestBranch()) {
      // just validate the latest block
      Right(true)
    } else {
      if (isHeavierThanBestBranch(branch)) {
        // create a queue of (branchTip, CancelableFuture)
        // if any branch is being executed at the moment while a better one comes is then call the cancellation hook
        attemptToSetNewBestBranch(branch) match {
          case Right(result) => // save pointer to new best branch
            Right(true)
          case Left(error) => Left(error)
        }
      } else {
        // nothing
        Right(true)
      }
    }

  private def extendsBestBranch(): Boolean = ???

  /** Compares the weight of the updatedBranch with the weight of the current best branch
    * @param updatedBranch
    * @return true if updatedBranch is heavier than current best branch, false otherwise
    */
  private def isHeavierThanBestBranch(updatedBranch: UpdatedBranch): Boolean = {
    val bestBlock = blockchainReader.getBestBlock()
    val bestHash = bestBlock.map(_.header.hash)
    val newBranchWeight = updatedBranch.weight

    bestHash
      .flatMap(blockchain.getChainWeightByHash)
      .exists(weight => newBranchWeight > weight.totalDifficulty)
  }

  /** Tries to set a new best branch by executing all blocks in the branch, from the HCB to the branch tip.
    * We assume the pre validation of the blocks of the branch was done already
    * @param branch
    * @return  Either[BlockExecutionError, Boolean]
    */
  private def attemptToSetNewBestBranch(branch: UpdatedBranch): Either[BlockExecutionError, Boolean] =
    // The Blocks of the branch won't be in a queue anymore so this needs to be re-written
    blockImport.reorganiseChainFromQueue(branch.tip)

}
