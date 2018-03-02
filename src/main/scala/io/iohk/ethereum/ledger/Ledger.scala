package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{TxsExecutionError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.ledger.Ledger._
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig, Logger}
import io.iohk.ethereum.vm._
import org.spongycastle.util.encoders.Hex

trait Ledger {
  def consensus: Consensus

  def checkBlockStatus(blockHash: ByteString): BlockStatus

  /**
   * Executes a block
   *
   * @param alreadyValidated should we skip pre-execution validation (if the block has already been validated,
   *                         eg. in the importBlock method)
   */
  def executeBlock(block: Block, alreadyValidated: Boolean = false): Either[BlockExecutionError, Seq[Receipt]]

  def prepareBlock(block: Block): BlockPreparationResult

  def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): TxResult

  /**
   * Tries to import the block as the new best block in the chain or enqueue it for later processing.
   *
   * The implementation uses [[io.iohk.ethereum.consensus.Consensus Consensus]] in order to apply
   * validation rules.
   *
   * @see [[io.iohk.ethereum.consensus.Consensus Consensus]],
   *      [[io.iohk.ethereum.consensus.validators.Validators Validators]]
   *
   * @param block - block to be imported
   * @return One of:
   *         - [[BlockImportedToTop]] - if the block was added as the new best block
   *         - [[BlockEnqueued]] - block is stored in the [[BlockQueue]]
   *         - [[ChainReorganised]] - a better new branch was found causing chain reorganisation
   *         - [[DuplicateBlock]] - block already exists either in the main chain or in the queue
   *         - [[BlockImportFailed]] - block failed to execute (when importing to top or reorganising the chain)
   */
  def importBlock(block: Block): BlockImportResult

  def resolveBranch(headers: Seq[BlockHeader]): BranchResolutionResult

  def binarySearchGasEstimation(stx: SignedTransaction, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): BigInt
}

//FIXME: Make Ledger independent of BlockchainImpl, for which it should become independent of WorldStateProxy type
//TODO: EC-313: this has grown a bit large, consider splitting the aspects block import, block exec and TX exec
// scalastyle:off number.of.methods
// scalastyle:off file.size.limit
/**
  * Ledger handles importing and executing blocks.
  * Note: this class thread-unsafe because of its dependencies on Blockchain and BlockQueue
  */
class LedgerImpl(
  blockchain: BlockchainImpl,
  blockQueue: BlockQueue,
  blockchainConfig: BlockchainConfig,
  theConsensus: Consensus
) extends Ledger with Logger {

  def this(
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    syncConfig: SyncConfig,
    theConsensus: Consensus
  ) =
    this(blockchain, BlockQueue(blockchain, syncConfig), blockchainConfig, theConsensus)

  private[this] val _blockPreparator = theConsensus.blockPreparator

  private[ledger] val blockRewardCalculator = _blockPreparator.blockRewardCalculator

  def consensus: Consensus = theConsensus

  // scalastyle:off method.length
  def importBlock(block: Block): BlockImportResult = {
    val validationResult = validateBlockBeforeExecution(block)
    validationResult match {
      case Left(ValidationBeforeExecError(HeaderParentNotFoundError)) =>
        val isGenesis = block.header.number == 0 && blockchain.genesisHeader.hash == block.header.hash
        if (isGenesis){
          log.debug(s"Ignoring duplicate genesis block: (${block.idTag})")
          DuplicateBlock
        } else {
          log.debug(s"Block(${block.idTag}) has no known parent")
          UnknownParent
        }

      case Left(ValidationBeforeExecError(reason)) =>
        log.debug(s"Block(${block.idTag}) failed pre-import validation")
        BlockImportFailed(reason.toString)

      case Right(_) =>
        val isDuplicate = blockchain.getBlockByHash(block.header.hash).isDefined || blockQueue.isQueued(block.header.hash)

        if (isDuplicate) {
          log.debug(s"Ignoring duplicate block: (${block.idTag})")
          DuplicateBlock
        }

        else {
          val bestBlock = blockchain.getBestBlock()
          val currentTd = blockchain.getTotalDifficultyByHash(bestBlock.header.hash).get

          val isTopOfChain = block.header.parentHash == bestBlock.header.hash

          if (isTopOfChain)
            importBlockToTop(block, bestBlock.header.number, currentTd)
          else
            enqueueBlockOrReorganiseChain(block, bestBlock, currentTd)
        }
    }
  }

  private def importBlockToTop(block: Block, bestBlockNumber: BigInt, currentTd: BigInt): BlockImportResult = {
    val topBlockHash = blockQueue.enqueueBlock(block, bestBlockNumber).get.hash
    val topBlocks = blockQueue.getBranch(topBlockHash, dequeue = true)
    val (importedBlocks, maybeError) = executeBlocks(topBlocks, currentTd)
    val totalDifficulties = importedBlocks.foldLeft(List(currentTd)) {(tds, b) =>
      (tds.head + b.header.difficulty) :: tds
    }.reverse.tail

    val result = maybeError match {
      case None =>
        BlockImportedToTop(importedBlocks, totalDifficulties)

      case Some(error) if importedBlocks.isEmpty =>
        blockQueue.removeSubtree(block.header.hash)
        BlockImportFailed(error.toString)

      case Some(error) =>
        topBlocks.drop(importedBlocks.length).headOption.foreach { failedBlock =>
          blockQueue.removeSubtree(failedBlock.header.hash)
        }
        BlockImportedToTop(importedBlocks, totalDifficulties)
    }

    importedBlocks.foreach { b =>
      log.debug(s"Imported new block (${b.header.number}: ${Hex.toHexString(b.header.hash.toArray)}) to the top of chain")
    }
    result
  }


  private def enqueueBlockOrReorganiseChain(block: Block, bestBlock: Block, currentTd: BigInt): BlockImportResult = {
    // compares the total difficulties of branches, and resolves the tie by gas if enabled
    // yes, apparently only the gas from last block is checked:
    // https://github.com/ethereum/cpp-ethereum/blob/develop/libethereum/BlockChain.cpp#L811
    def isBetterBranch(newTd: BigInt) =
    newTd > currentTd ||
      (blockchainConfig.gasTieBreaker && newTd == currentTd && block.header.gasUsed > bestBlock.header.gasUsed)

    blockQueue.enqueueBlock(block, bestBlock.header.number) match {
      case Some(Leaf(leafHash, leafTd)) if isBetterBranch(leafTd) =>
        log.debug("Found a better chain, about to reorganise")
        reorganiseChainFromQueue(leafHash) match {
          case Right((oldBranch, newBranch)) =>
            val totalDifficulties = newBranch.tail.foldRight(List(leafTd)) { (b, tds) =>
              (tds.head - b.header.difficulty) :: tds
            }
            ChainReorganised(oldBranch, newBranch, totalDifficulties)

          case Left(error) =>
            BlockImportFailed(s"Error while trying to reorganise chain: $error")
        }

      case _ =>
        BlockEnqueued
    }
  }

  /**
    * Once a better branch was found this attempts to reorganise the chain
    * @param queuedLeaf - a block hash that determines a new branch stored in the queue (newest block from the branch)
    * @return [[BlockExecutionError]] if one of the blocks in the new branch failed to execute, otherwise:
    *        (oldBranch, newBranch) as lists of blocks
    */
  private def reorganiseChainFromQueue(queuedLeaf: ByteString): Either[BlockExecutionError, (List[Block], List[Block])] = {
    val newBranch = blockQueue.getBranch(queuedLeaf, dequeue = true)
    val parent = newBranch.head.header.parentHash
    val bestNumber = blockchain.getBestBlockNumber()
    val parentTd = blockchain.getTotalDifficultyByHash(parent).get

    val staleBlocksWithReceiptsAndTDs = removeBlocksUntil(parent, bestNumber).reverse
    val staleBlocks = staleBlocksWithReceiptsAndTDs.map(_._1)

    for (block <- staleBlocks) yield blockQueue.enqueueBlock(block)

    val (executedBlocks, maybeError) = executeBlocks(newBranch, parentTd)
    maybeError match {
      case None =>
        Right(staleBlocks, executedBlocks)

      case Some(error) =>
        revertChainReorganisation(newBranch, staleBlocksWithReceiptsAndTDs, executedBlocks)
        Left(error)
    }
  }

  /**
    * Used to revert chain reorganisation in the event that one of the blocks from new branch
    * fails to execute
    *
    * @param newBranch - new blocks
    * @param oldBranch - old blocks along with corresponding receipts and totalDifficulties
    * @param executedBlocks - sub-sequence of new branch that was executed correctly
    */
  private def revertChainReorganisation(newBranch: List[Block], oldBranch: List[(Block, Seq[Receipt], BigInt)],
    executedBlocks: List[Block]): Unit = {

    if (executedBlocks.nonEmpty) {
      removeBlocksUntil(executedBlocks.head.header.parentHash, executedBlocks.last.header.number)
    }

    oldBranch.foreach { case (block, receipts, td) =>
      blockchain.save(block, receipts, td, saveAsBestBlock = false)
    }

    val bestNumber = oldBranch.last._1.header.number
    blockchain.saveBestBlockNumber(bestNumber)
    executedBlocks.foreach(blockQueue.enqueueBlock(_, bestNumber))

    newBranch.diff(executedBlocks).headOption.foreach { block =>
      blockQueue.removeSubtree(block.header.hash)
    }
  }

  /**
    * Executes a list blocks, storing the results in the blockchain
    * @param blocks block to be executed
    * @return a list of blocks that were correctly executed and an optional [[BlockExecutionError]]
    */
  private def executeBlocks(blocks: List[Block], parentTd: BigInt): (List[Block], Option[BlockExecutionError]) = {
    blocks match {
      case block :: remainingBlocks =>
        executeBlock(block, alreadyValidated = true) match {
          case Right (receipts) =>
            val td = parentTd + block.header.difficulty
            blockchain.save(block, receipts, td, saveAsBestBlock = true)

            val (executedBlocks, error) = executeBlocks(remainingBlocks, td)
            (block :: executedBlocks, error)

          case Left(error) =>
          (Nil, Some(error))
        }

      case Nil =>
        (Nil, None)
    }
  }

  /**
    * Remove blocks from the [[Blockchain]] along with receipts and total difficulties
    * @param parent remove blocks until this hash (exclusive)
    * @param fromNumber start removing from this number (downwards)
    * @return the list of removed blocks along with receipts and total difficulties
    */
  private def removeBlocksUntil(parent: ByteString, fromNumber: BigInt): List[(Block, Seq[Receipt], BigInt)] = {
    blockchain.getBlockByNumber(fromNumber) match {
      case Some(block) if block.header.hash == parent =>
        Nil

      case Some(block) =>
        val receipts = blockchain.getReceiptsByHash(block.header.hash).get
        val td = blockchain.getTotalDifficultyByHash(block.header.hash).get

        //not updating best block number for efficiency, it will be updated in the callers anyway
        blockchain.removeBlock(block.header.hash, saveParentAsBestBlock = false)
        (block, receipts, td) :: removeBlocksUntil(parent, fromNumber - 1)

      case None =>
        log.error(s"Unexpected missing block number: $fromNumber")
        Nil
    }
  }

  /**
    * Finds a relation of a given list of headers to the current chain
    * Note:
    *   - the headers should form a chain (headers ordered by number)
    *   - last header number should be greater or equal than current best block number
    * @param headers - a list of headers to be checked
    * @return One of:
    *         - [[NewBetterBranch]] - the headers form a better branch than our current main chain
    *         - [[NoChainSwitch]] - the headers do not form a better branch
    *         - [[UnknownBranch]] - the parent of the first header is unknown (caller should obtain more headers)
    *         - [[InvalidBranch]] - headers do not form a chain or last header number is less than current best block number
    */
  def resolveBranch(headers: Seq[BlockHeader]): BranchResolutionResult = {
    if (!doHeadersFormChain(headers) || headers.last.number < blockchain.getBestBlockNumber())
      InvalidBranch
    else {
      val parentIsKnown = blockchain.getBlockHeaderByHash(headers.head.parentHash).isDefined

      // dealing with a situation when genesis block is included in the received headers, which may happen
      // in the early block of private networks
      val reachedGenesis = headers.head.number == 0 && blockchain.getBlockHeaderByNumber(0).get.hash == headers.head.hash

      if (parentIsKnown || reachedGenesis) {
        // find blocks with same numbers in the current chain, removing any common prefix
        val (oldBranch, _) = getBlocksForHeaders(headers).zip(headers)
          .dropWhile{ case (oldBlock, newHeader) => oldBlock.header == newHeader }.unzip
        val newHeaders = headers.dropWhile(h => oldBranch.headOption.exists(_.header.number > h.number))

        val currentBranchDifficulty = oldBranch.map(_.header.difficulty).sum
        val newBranchDifficulty = newHeaders.map(_.difficulty).sum

        if (currentBranchDifficulty < newBranchDifficulty)
          NewBetterBranch(oldBranch)
        else
          NoChainSwitch
      }
      else
        UnknownBranch
    }
  }

  private def doHeadersFormChain(headers: Seq[BlockHeader]): Boolean =
    if (headers.length > 1)
      headers.zip(headers.tail).forall {
        case (parent, child) =>
          parent.hash == child.parentHash && parent.number + 1 == child.number
      }
    else
      headers.nonEmpty

  private def getBlocksForHeaders(headers: Seq[BlockHeader]): List[Block] = headers match {
    case Seq(h, tail @ _*) =>
      blockchain.getBlockByNumber(h.number).map(_ :: getBlocksForHeaders(tail)).getOrElse(Nil)
    case Seq() =>
      Nil
  }
  /**
    * Check current status of block, based on its hash
    *
    * @param blockHash - hash of block to check
    * @return One of:
    *         - [[InChain]] - Block already incorporated into blockchain
    *         - [[Queued]]  - Block in queue waiting to be resolved
    *         - [[UnknownBlock]] - Hash its not known to our client
    */
  def checkBlockStatus(blockHash: ByteString): BlockStatus = {
    if (blockchain.getBlockByHash(blockHash).isDefined)
      InChain
    else if (blockQueue.isQueued(blockHash))
      Queued
    else
      UnknownBlock
  }

  def executeBlock(block: Block, alreadyValidated: Boolean = false): Either[BlockExecutionError, Seq[Receipt]] = {

    val preExecValidationResult = if (alreadyValidated) Right(block) else validateBlockBeforeExecution(block)

    val blockExecResult = for {
      _ <- preExecValidationResult

      execResult <- executeBlockTransactions(block)
      BlockResult(resultingWorldStateProxy, gasUsed, receipts) = execResult
      worldToPersist = payBlockReward(block, resultingWorldStateProxy)
      worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist) //State root hash needs to be up-to-date for validateBlockAfterExecution

      _ <- validateBlockAfterExecution(block, worldPersisted.stateRootHash, receipts, gasUsed)

    } yield receipts

    if(blockExecResult.isRight)
      log.debug(s"Block ${block.header.number} (with hash: ${block.header.hashAsHexString}) executed correctly")
    blockExecResult
  }

  def prepareBlock(block: Block): BlockPreparationResult = {
    _blockPreparator.prepareBlock(block)
  }

  /**
    * This function runs transaction
    *
    * @param block
    */
  private[ledger] def executeBlockTransactions(block: Block):
  Either[BlockExecutionError, BlockResult] = {
    val parentStateRoot = blockchain.getBlockHeaderByHash(block.header.parentHash).map(_.stateRoot)
    val initialWorld =
      blockchain.getWorldStateProxy(
        block.header.number,
        blockchainConfig.accountStartNonce,
        parentStateRoot,
        EvmConfig.forBlock(block.header.number, blockchainConfig).noEmptyAccounts)

    val inputWorld = blockchainConfig.daoForkConfig match {
      case Some(daoForkConfig) if daoForkConfig.isDaoForkBlock(block.header.number) => drainDaoForkAccounts(initialWorld, daoForkConfig)
      case _ => initialWorld
    }

    log.debug(s"About to execute ${block.body.transactionList.size} txs from block ${block.header.number} (with hash: ${block.header.hashAsHexString})")
    val blockTxsExecResult = executeTransactions(block.body.transactionList, inputWorld, block.header)
    blockTxsExecResult match {
      case Right(_) => log.debug(s"All txs from block ${block.header.hashAsHexString} were executed successfully")
      case Left(error) => log.debug(s"Not all txs from block ${block.header.hashAsHexString} were executed correctly, due to ${error.reason}")
    }
    blockTxsExecResult
  }

  private[ledger] final def executePreparedTransactions(
    signedTransactions: Seq[SignedTransaction],
    world: InMemoryWorldStateProxy,
    blockHeader: BlockHeader,
    acumGas: BigInt = 0,
    acumReceipts: Seq[Receipt] = Nil,
    executed: Seq[SignedTransaction] = Nil
  ): (BlockResult, Seq[SignedTransaction]) =
    _blockPreparator.executePreparedTransactions(
      signedTransactions = signedTransactions,
      world = world,
      blockHeader = blockHeader,
      acumGas = acumGas,
      acumReceipts = acumReceipts,
      executed = executed
    )

  private[ledger] final def executeTransactions(
    signedTransactions: Seq[SignedTransaction],
    world: InMemoryWorldStateProxy,
    blockHeader: BlockHeader,
    acumGas: BigInt = 0,
    acumReceipts: Seq[Receipt] = Nil
  ): Either[TxsExecutionError, BlockResult] =
    _blockPreparator.executeTransactions(
      signedTransactions = signedTransactions,
      world = world,
      blockHeader = blockHeader,
      acumGas = acumGas,
      acumReceipts = acumReceipts
    )

  override def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): TxResult = {

    val world1 = world.getOrElse(blockchain.getReadOnlyWorldStateProxy(None, blockchainConfig.accountStartNonce, Some(blockHeader.stateRoot)))

    val world2 =
      if (world1.getAccount(stx.senderAddress).isEmpty)
        world1.saveAccount(stx.senderAddress, Account.empty(blockchainConfig.accountStartNonce))
      else
        world1

    val worldForTx = updateSenderAccountBeforeExecution(stx, world2)

    val context: PC = prepareProgramContext(stx, blockHeader, worldForTx)

    val result = runVM(stx, context)

    val totalGasToRefund = calcTotalGasToRefund(stx, result)

    TxResult(result.world, stx.tx.gasLimit - totalGasToRefund, result.logs, result.returnData, result.error)
  }

  override def binarySearchGasEstimation(stx: SignedTransaction, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): BigInt = {
    val lowLimit = EvmConfig.forBlock(blockHeader.number, blockchainConfig).feeSchedule.G_transaction
    val highLimit = stx.tx.gasLimit

    if (highLimit < lowLimit)
      highLimit
    else {
      LedgerUtils.binaryChop(lowLimit, highLimit)(gasLimit =>
        simulateTransaction(stx.copy(tx = stx.tx.copy(gasLimit = gasLimit)), blockHeader, world).vmError)
    }
  }

  private[ledger] def executeTransaction(
    stx: SignedTransaction,
    blockHeader: BlockHeader,
    world: InMemoryWorldStateProxy
  ): TxResult =
    _blockPreparator.executeTransaction(
      stx = stx,
      blockHeader = blockHeader,
      world = world
    )

  private def validateBlockBeforeExecution(block: Block): Either[ValidationBeforeExecError, BlockExecutionSuccess] = {
    consensus.validateBlockBeforeExecution(
      block = block,
      getBlockHeaderByHash = getHeaderFromChainOrQueue,
      getNBlocksBack = getNBlocksBackFromChainOrQueue
    )
  }

  private[ledger] def validateBlockAfterExecution(
    block: Block,
    stateRootHash: ByteString,
    receipts: Seq[Receipt],
    gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess] = {

    consensus.validateBlockAfterExecution(
      block = block,
      stateRootHash = stateRootHash,
      receipts = receipts,
      gasUsed = gasUsed
    )
  }

  private[ledger] def payBlockReward(block: Block, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
    _blockPreparator.payBlockReward(block, worldStateProxy)

  private def updateSenderAccountBeforeExecution(
    stx: SignedTransaction,
    worldStateProxy: InMemoryWorldStateProxy
  ): InMemoryWorldStateProxy =
    _blockPreparator.updateSenderAccountBeforeExecution(stx, worldStateProxy)

  private def prepareProgramContext(
    stx: SignedTransaction,
    blockHeader: BlockHeader,
    world: InMemoryWorldStateProxy
  ): PC =
    _blockPreparator.prepareProgramContext(
      stx = stx,
      blockHeader = blockHeader,
      world = world
    )

  private def runVM(stx: SignedTransaction, context: PC): PR =
    _blockPreparator.runVM(stx, context)

  private[ledger] def saveNewContract(address: Address, result: PR, config: EvmConfig): PR =
    _blockPreparator.saveNewContract(
      address = address,
      result = result,
      config = config
    )

  private def calcTotalGasToRefund(stx: SignedTransaction, result: PR): BigInt =
    _blockPreparator.calcTotalGasToRefund(stx, result)

  private[ledger] def pay(address: Address, value: UInt256)(world: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
    _blockPreparator.pay(address, value)(world)

  private[ledger] def deleteAccounts(addressesToDelete: Set[Address])(worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
    _blockPreparator.deleteAccounts(addressesToDelete)(worldStateProxy)

  /**
    * This function updates worldState transferring balance from drainList accounts to refundContract address
    *
    * @param worldState Initial world state
    * @param daoForkConfig Dao fork configuration with drainList and refundContract config
    * @return Updated world state proxy
    */
  private def drainDaoForkAccounts(worldState: InMemoryWorldStateProxy, daoForkConfig: DaoForkConfig): InMemoryWorldStateProxy = {

    daoForkConfig.refundContract match {
      case Some(refundContractAddress) =>
        daoForkConfig.drainList.foldLeft(worldState) { (ws, address) =>
          ws.getAccount(address)
            .map(account => ws.transfer(from = address, to = refundContractAddress, account.balance))
            .getOrElse(ws)
        }
      case None => worldState
    }
  }

  private[ledger] def deleteEmptyTouchedAccounts(world: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
    _blockPreparator.deleteEmptyTouchedAccounts(world)

  private def getHeaderFromChainOrQueue(hash: ByteString): Option[BlockHeader] =
    blockchain.getBlockHeaderByHash(hash).orElse(blockQueue.getBlockByHash(hash).map(_.header))

  private def getNBlocksBackFromChainOrQueue(hash: ByteString, n: Int): List[Block] = {
    val queuedBlocks = blockQueue.getBranch(hash, dequeue = false).take(n)
    if (queuedBlocks.length == n)
      queuedBlocks
    else {
      val chainedBlockHash = queuedBlocks.headOption.map(_.header.parentHash).getOrElse(hash)
      blockchain.getBlockByHash(chainedBlockHash) match {
        case None =>
          Nil

        case Some(block) =>
          val remaining = n - queuedBlocks.length - 1
          val numbers = (block.header.number - remaining) until block.header.number
          (numbers.toList.flatMap(blockchain.getBlockByNumber) :+ block) ::: queuedBlocks
      }
    }
  }
}

object Ledger {
  type PC = ProgramContext[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PR = ProgramResult[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]

  case class BlockResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt = 0, receipts: Seq[Receipt] = Nil)
  case class BlockPreparationResult(block: Block, blockResult: BlockResult, stateRootHash: ByteString, updatedWorld: InMemoryWorldStateProxy)
  case class TxResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt, logs: Seq[TxLogEntry],
    vmReturnData: ByteString, vmError: Option[ProgramError])
}

sealed trait BlockExecutionError{
  val reason: Any
}

sealed trait BlockExecutionSuccess
case object BlockExecutionSuccess extends BlockExecutionSuccess

object BlockExecutionError {
  case class ValidationBeforeExecError(reason: Any) extends BlockExecutionError
  case class StateBeforeFailure(worldState: InMemoryWorldStateProxy, acumGas: BigInt, acumReceipts: Seq[Receipt])
  case class TxsExecutionError(stx: SignedTransaction, stateBeforeError: StateBeforeFailure, reason: String) extends BlockExecutionError
  case class ValidationAfterExecError(reason: String) extends BlockExecutionError
}

sealed trait BlockImportResult
case class BlockImportedToTop(imported: List[Block], totalDifficulties: List[BigInt]) extends BlockImportResult
case object BlockEnqueued extends BlockImportResult
case object DuplicateBlock extends BlockImportResult
case class ChainReorganised(oldBranch: List[Block], newBranch: List[Block], totalDifficulties: List[BigInt]) extends BlockImportResult
case class BlockImportFailed(error: String) extends BlockImportResult
case object UnknownParent extends BlockImportResult

sealed trait BranchResolutionResult
case class  NewBetterBranch(oldBranch: Seq[Block]) extends BranchResolutionResult
case object NoChainSwitch extends BranchResolutionResult
case object UnknownBranch extends BranchResolutionResult
case object InvalidBranch extends BranchResolutionResult

sealed trait BlockStatus
case object InChain       extends BlockStatus
case object Queued        extends BlockStatus
case object UnknownBlock  extends BlockStatus

trait BlockPreparationError

case class TxError(reason: String) extends BlockPreparationError
