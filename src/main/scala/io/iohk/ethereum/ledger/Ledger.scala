package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.Ledger._
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.{ BlockchainConfig, Logger }
import io.iohk.ethereum.vm._

import scala.concurrent.{ ExecutionContext, Future }

trait Ledger {
  def consensus: Consensus

  def checkBlockStatus(blockHash: ByteString): BlockStatus

  /** Executes a block
    *
    * @param alreadyValidated should we skip pre-execution validation (if the block has already been validated,
    *                         eg. in the importBlock method)
    */
  def executeBlock(block: Block, alreadyValidated: Boolean = false): Either[BlockExecutionError, Seq[Receipt]]

  def simulateTransaction(stx: SignedTransactionWithSender, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): TxResult

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
  def importBlock(block: Block)(implicit blockExecutionContext: ExecutionContext): Future[BlockImportResult]

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
  def resolveBranch(headers: Seq[BlockHeader]): BranchResolutionResult

  def binarySearchGasEstimation(stx: SignedTransactionWithSender, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): BigInt
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
  validationContext: ExecutionContext
) extends Ledger with Logger {

  def this(
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    syncConfig: SyncConfig,
    theConsensus: Consensus,
    validationContext: ExecutionContext
  ) = this(blockchain, BlockQueue(blockchain, syncConfig), blockchainConfig, theConsensus, validationContext)

  private[this] val _blockPreparator = theConsensus.blockPreparator

  private[ledger] val blockRewardCalculator = _blockPreparator.blockRewardCalculator

  private[ledger] val blockValidation = new BlockValidation(consensus, blockchain, blockQueue)
  private[ledger] val blockImport =
    new BlockImport(blockchain, blockQueue, blockchainConfig, executeBlock, blockValidation, validationContext)
  private[ledger] val branchResolution = new BranchResolution(blockchain)
  private[ledger] val blockExecution = new BlockExecution(blockchain, blockchainConfig, _blockPreparator, consensus, executeBlock)

  override def consensus: Consensus = theConsensus

  /** Checks current status of block, based on its hash
    *
    * @param blockHash the hash of block to check
    * @return One of:
    *         - [[InChain]] - Block already incorporated into blockchain
    *         - [[Queued]]  - Block in queue waiting to be resolved
    *         - [[UnknownBlock]] - Hash its not known to our client
    */
  override def checkBlockStatus(blockHash: ByteString): BlockStatus = {
    if (blockchain.getBlockByHash(blockHash).isDefined)
      InChain
    else if (blockQueue.isQueued(blockHash))
      Queued
    else
      UnknownBlock
  }

  override def executeBlock(block: Block, alreadyValidated: Boolean = false): Either[BlockExecutionError, Seq[Receipt]] = {

    val preExecValidationResult = if (alreadyValidated) Right(block) else blockValidation.validateBlockBeforeExecution(block)

    val blockExecResult = for {
      _ <- preExecValidationResult

      execResult <- blockExecution.executeBlockTransactions(block)
      BlockResult(resultingWorldStateProxy, gasUsed, receipts) = execResult
      worldToPersist = blockExecution.payBlockReward(block, resultingWorldStateProxy)
      // State root hash needs to be up-to-date for validateBlockAfterExecution
      worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist)
      _ <- blockValidation.validateBlockAfterExecution(block, worldPersisted.stateRootHash, receipts, gasUsed)

    } yield receipts

    if(blockExecResult.isRight)
      log.debug(s"Block ${block.header.number} (with hash: ${block.header.hashAsHexString}) executed correctly")
    blockExecResult
  }

  override def simulateTransaction(stx: SignedTransactionWithSender, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): TxResult = {
    val tx = stx.tx

    val world1 = world.getOrElse(blockchain.getReadOnlyWorldStateProxy(None, blockchainConfig.accountStartNonce, Some(blockHeader.stateRoot),
      noEmptyAccounts = false,
      ethCompatibleStorage = blockchainConfig.ethCompatibleStorage))

    val world2 =
      if (world1.getAccount(stx.senderAddress).isEmpty)
        world1.saveAccount(stx.senderAddress, Account.empty(blockchainConfig.accountStartNonce))
      else
        world1

    val worldForTx = _blockPreparator.updateSenderAccountBeforeExecution(tx, stx.senderAddress,  world2)

    val result = _blockPreparator.runVM(tx, stx.senderAddress, blockHeader, worldForTx)

    val totalGasToRefund = _blockPreparator.calcTotalGasToRefund(tx, result)

    TxResult(result.world, stx.tx.tx.gasLimit - totalGasToRefund, result.logs, result.returnData, result.error)
  }

  override def importBlock(block: Block)(implicit blockExecutionContext: ExecutionContext): Future[BlockImportResult] = {

    val currentBestBlock = blockchain.getBestBlock()

    if (isBlockADuplicate(block.header, currentBestBlock.header.number)) {
      log.debug(s"Ignoring duplicate block: (${block.idTag})")
      Future.successful(DuplicateBlock)
    } else {
      val hash = currentBestBlock.header.hash
      blockchain.getTotalDifficultyByHash(hash) match {
        case Some(currentTd) =>
          if (isPossibleNewBestBlock(block.header, currentBestBlock.header)) {
            blockImport.importToTop(block, currentBestBlock, currentTd)(blockExecutionContext)
          } else {
            blockImport.reorganise(block, currentBestBlock, currentTd)
          }

        case None =>
          Future.successful(BlockImportFailed(s"Couldn't get total difficulty for current best block with hash: $hash"))

      }
    }
  }

  private def isBlockADuplicate(block: BlockHeader, currentBestBlockNumber: BigInt): Boolean = {
    val hash = block.hash
    blockchain.getBlockByHash(hash).isDefined && block.number <= currentBestBlockNumber || blockQueue.isQueued(hash)
  }

  private def isPossibleNewBestBlock(newBlock: BlockHeader, currentBestBlock: BlockHeader): Boolean =
    newBlock.parentHash == currentBestBlock.hash && newBlock.number == currentBestBlock.number + 1

  override def resolveBranch(headers: Seq[BlockHeader]): BranchResolutionResult = branchResolution.resolveBranch(headers)

  override def binarySearchGasEstimation(stx: SignedTransactionWithSender, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): BigInt = {
    val lowLimit = EvmConfig.forBlock(blockHeader.number, blockchainConfig).feeSchedule.G_transaction
    val tx = stx.tx
    val highLimit = tx.tx.gasLimit

    if (highLimit < lowLimit) {
      highLimit
    } else {
      LedgerUtils.binaryChop(lowLimit, highLimit){ gasLimit =>
        simulateTransaction(stx.copy(tx = tx.copy(tx = tx.tx.copy(gasLimit = gasLimit))), blockHeader, world).vmError
      }
    }
  }
}

object Ledger {
  type VMImpl = VM[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PC = ProgramContext[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PR = ProgramResult[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]

  case class BlockResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt = 0, receipts: Seq[Receipt] = Nil)
  case class BlockPreparationResult(block: Block, blockResult: BlockResult, stateRootHash: ByteString, updatedWorld: InMemoryWorldStateProxy)
  case class TxResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt, logs: Seq[TxLogEntry],
    vmReturnData: ByteString, vmError: Option[ProgramError])
}

case class BlockData(block: Block, receipts: Seq[Receipt], td: BigInt)

sealed trait BlockStatus
case object InChain       extends BlockStatus
case object Queued        extends BlockStatus
case object UnknownBlock  extends BlockStatus

trait BlockPreparationError

case class TxError(reason: String) extends BlockPreparationError
