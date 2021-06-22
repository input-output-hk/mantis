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
  def consensus: Consensus

  /** Checks current status of block, based on its hash
    *
    * @param blockHash the hash of block to check
    * @return One of:
    *         - [[io.iohk.ethereum.ledger.InChain]] - Block already incorporated into blockchain
    *         - [[io.iohk.ethereum.ledger.Queued]]  - Block in queue waiting to be resolved
    *         - [[io.iohk.ethereum.ledger.UnknownBlock]] - Hash its not known to our client
    */
  def checkBlockStatus(blockHash: ByteString): BlockStatus

  /**
    * Returns a block if it's either stored in the blockchain or enqueued
    */
  def getBlockByHash(hash: ByteString): Option[Block]

  /** Tries to import the block as the new best block in the chain or enqueue it for later processing.
    *
    * The implementation uses [[io.iohk.ethereum.consensus.Consensus]] in order to apply
    * validation rules.
    *
    * @see [[io.iohk.ethereum.consensus.Consensus]], [[io.iohk.ethereum.consensus.validators.Validators]]
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
  def importBlock(block: Block)(implicit blockExecutionScheduler: Scheduler): Task[BlockImportResult]

  /** Finds a relation of a given list of headers to the current chain
    *
    * @note
    *   - the headers should form a chain (headers ordered by number)
    *   - last header number should be greater or equal than current best block number
    *
    * @param headers - a list of headers to be checked
    * @return One of:
    *         - [[io.iohk.ethereum.ledger.NewBetterBranch]] - the headers form a better branch than our current main chain
    *         - [[io.iohk.ethereum.ledger.NoChainSwitch]] - the headers do not form a better branch
    *         - [[io.iohk.ethereum.ledger.UnknownBranch]] - the parent of the first header is unknown (caller should obtain more headers)
    *         - [[io.iohk.ethereum.ledger.InvalidBranch]] - headers do not form a chain or last header number is less than current best block number
    */
  def resolveBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult

}

//FIXME: Make Ledger independent of BlockchainImpl, for which it should become independent of WorldStateProxy type
// scalastyle:off number.of.methods file.size.limit
/** Ledger handles importing and executing blocks.
  *
  * @note this class thread-unsafe because of its dependencies on [[io.iohk.ethereum.domain.Blockchain]] and [[io.iohk.ethereum.ledger.BlockQueue]]
  */
class LedgerImpl(
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    blockQueue: BlockQueue,
    blockchainConfig: BlockchainConfig,
    theConsensus: Consensus,
    validationContext: Scheduler
) extends Ledger
    with Logger {

  def this(
      blockchain: BlockchainImpl,
      blockchainReader: BlockchainReader,
      blockchainConfig: BlockchainConfig,
      syncConfig: SyncConfig,
      theConsensus: Consensus,
      validationContext: Scheduler
  ) = this(
    blockchain,
    blockchainReader,
    BlockQueue(blockchain, syncConfig),
    blockchainConfig,
    theConsensus,
    validationContext
  )

  val consensus: Consensus = theConsensus

  private[this] val _blockPreparator = theConsensus.blockPreparator

  private[ledger] val blockRewardCalculator = _blockPreparator.blockRewardCalculator

  private[ledger] lazy val blockValidation = new BlockValidation(consensus, blockchainReader, blockQueue)
  private[ledger] lazy val blockExecution =
    new BlockExecution(blockchain, blockchainReader, blockchainConfig, consensus.blockPreparator, blockValidation)
  private[ledger] val branchResolution = new BranchResolution(blockchain, blockchainReader)
  private[ledger] val blockImport =
    new BlockImport(
      blockchain,
      blockchainReader,
      blockQueue,
      blockValidation,
      blockExecution,
      validationContext
    )

  override def checkBlockStatus(blockHash: ByteString): BlockStatus = {
    if (blockchainReader.getBlockByHash(blockHash).isDefined)
      InChain
    else if (blockQueue.isQueued(blockHash))
      Queued
    else
      UnknownBlock
  }

  override def getBlockByHash(hash: ByteString): Option[Block] =
    blockchainReader.getBlockByHash(hash) orElse blockQueue.getBlockByHash(hash)

  override def importBlock(
      block: Block
  )(implicit blockExecutionScheduler: Scheduler): Task[BlockImportResult] =
    blockchain.getBestBlock() match {
      case Some(bestBlock) =>
        if (isBlockADuplicate(block.header, bestBlock.header.number)) {
          Task(log.debug(s"Ignoring duplicate block: (${block.idTag})"))
            .map(_ => DuplicateBlock)
        } else {
          val hash = bestBlock.header.hash
          blockchain.getChainWeightByHash(hash) match {
            case Some(weight) =>
              val importResult = if (isPossibleNewBestBlock(block.header, bestBlock.header)) {
                blockImport.importToTop(block, bestBlock, weight)
              } else {
                blockImport.reorganise(block, bestBlock, weight)
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
                  "Couldn't get total difficulty for current best block with hash: ${bestBlock.header.hashAsHexString}"
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
    blockchainReader.getBlockByHash(hash).isDefined &&
    block.number <= currentBestBlockNumber || blockQueue.isQueued(hash)
  }

  private def isPossibleNewBestBlock(newBlock: BlockHeader, currentBestBlock: BlockHeader): Boolean =
    newBlock.parentHash == currentBestBlock.hash && newBlock.number == currentBestBlock.number + 1

  override def resolveBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult =
    branchResolution.resolveBranch(headers)

  private def measureBlockMetrics(importResult: BlockImportResult): Unit = {
    importResult match {
      case BlockImportedToTop(blockImportData) =>
        blockImportData.foreach(blockData => BlockMetrics.measure(blockData.block, blockchainReader.getBlockByHash))
      case ChainReorganised(_, newBranch, _) =>
        newBranch.foreach(block => BlockMetrics.measure(block, blockchainReader.getBlockByHash))
      case _ => ()
    }
  }

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
