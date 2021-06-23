package io.iohk.ethereum.ledger

import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.domain._
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.vm._
import monix.eval.Task
import monix.execution.Scheduler

trait Ledger {
  // /** Tries to import the block as the new best block in the chain or enqueue it for later processing.
  //   *
  //   * The implementation uses [[io.iohk.ethereum.consensus.Consensus]] in order to apply
  //   * validation rules.
  //   *
  //   * @see [[io.iohk.ethereum.consensus.Consensus]], [[io.iohk.ethereum.consensus.validators.Validators]]
  //   *
  //   * @param block                 block to be imported
  //   * @param blockExecutionContext threadPool on which the execution should be run
  //   * @return One of:
  //   *         - [[io.iohk.ethereum.ledger.BlockImportedToTop]] - if the block was added as the new best block
  //   *         - [[io.iohk.ethereum.ledger.BlockEnqueued]] - block is stored in the [[io.iohk.ethereum.ledger.BlockQueue]]
  //   *         - [[io.iohk.ethereum.ledger.ChainReorganised]] - a better new branch was found causing chain reorganisation
  //   *         - [[io.iohk.ethereum.ledger.DuplicateBlock]] - block already exists either in the main chain or in the queue
  //   *         - [[io.iohk.ethereum.ledger.BlockImportFailed]] - block failed to execute (when importing to top or reorganising the chain)
  //   */
  // def importBlock(block: Block)(implicit blockExecutionScheduler: Scheduler): Task[BlockImportResult]

}
