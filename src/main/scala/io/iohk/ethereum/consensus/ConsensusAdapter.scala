package io.iohk.ethereum.consensus

import io.iohk.ethereum.blockchain.sync.regular.BlockImportResult
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.utils.BlockchainConfig
import monix.eval.Task
import monix.execution.Scheduler

/** This is a temporary class to isolate the real Consensus and extract responsibilities which should not
  * be part of the consensus in the final design, but are currently needed.
  */
class ConsensusAdapter(consensus: Consensus) {
  def evaluateBranchBlock(
      block: Block
  )(implicit blockExecutionScheduler: Scheduler, blockchainConfig: BlockchainConfig): Task[BlockImportResult] =
    consensus.evaluateBranch(Seq(block))
}
