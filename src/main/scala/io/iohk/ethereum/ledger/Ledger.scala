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

//FIXME: Make Ledger independent of BlockchainImpl, for which it should become independent of WorldStateProxy type
// scalastyle:off number.of.methods file.size.limit
/** Ledger handles importing and executing blocks.
  *
  * @note this class thread-unsafe because of its dependencies on [[io.iohk.ethereum.domain.Blockchain]] and [[io.iohk.ethereum.ledger.BlockQueue]]
  */
class LedgerImpl(
    blockchain: BlockchainImpl,
    blockQueue: BlockQueue,
    blockchainConfig: BlockchainConfig,
    theConsensus: Consensus,
    validationContext: Scheduler
) extends Ledger
    with Logger {

  def this(
      blockchain: BlockchainImpl,
      blockchainConfig: BlockchainConfig,
      syncConfig: SyncConfig,
      theConsensus: Consensus,
      validationContext: Scheduler
  ) = this(blockchain, BlockQueue(blockchain, syncConfig), blockchainConfig, theConsensus, validationContext)

  val consensus: Consensus = theConsensus

  private[this] val _blockPreparator = theConsensus.blockPreparator

  private[ledger] val blockRewardCalculator = _blockPreparator.blockRewardCalculator

  private[ledger] lazy val blockValidation = new BlockValidation(consensus, blockchain, blockQueue)
  private[ledger] lazy val blockExecution =
    new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)
  // private[ledger] val branchResolution = new BranchResolution(blockchain)
  private[ledger] val blockImport =
    new BlockImport(
      blockchain,
      blockQueue,
      blockValidation,
      blockExecution,
      validationContext
    )

  // override

}

object Ledger {
  type VMImpl = VM[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PC = ProgramContext[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PR = ProgramResult[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]

  case class BlockResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt = 0, receipts: Seq[Receipt] = Nil)
  case class PreparedBlock(
      block: Block,
      blockResult: BlockResult,
      stateRootHash: ByteString,
      updatedWorld: InMemoryWorldStateProxy
  )
  case class TxResult(
      worldState: InMemoryWorldStateProxy,
      gasUsed: BigInt,
      logs: Seq[TxLogEntry],
      vmReturnData: ByteString,
      vmError: Option[ProgramError]
  )
}

case class BlockData(block: Block, receipts: Seq[Receipt], weight: ChainWeight)

sealed trait BlockStatus
case object InChain extends BlockStatus
case object Queued extends BlockStatus
case object UnknownBlock extends BlockStatus
