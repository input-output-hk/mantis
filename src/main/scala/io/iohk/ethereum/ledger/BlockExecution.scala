package io.iohk.ethereum.ledger

import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.MissingParentError
import io.iohk.ethereum.ledger.Ledger.BlockResult
import io.iohk.ethereum.utils.{BlockchainConfig, ByteStringUtils, DaoForkConfig, Logger}
import io.iohk.ethereum.vm.EvmConfig

import scala.annotation.tailrec
import cats.implicits._
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException

class BlockExecution(
    blockchain: BlockchainImpl,
    evmCodeStorage: EvmCodeStorage,
    blockchainConfig: BlockchainConfig,
    blockPreparator: BlockPreparator,
    blockValidation: BlockValidation
) extends Logger {

  /** Executes and validate a block
    *
    * @param alreadyValidated should we skip pre-execution validation (if the block has already been validated,
    *                         eg. in the importBlock method)
    */
  def executeAndValidateBlock(
      block: Block,
      alreadyValidated: Boolean = false
  ): Either[BlockExecutionError, Seq[Receipt]] = {
    val preExecValidationResult =
      if (alreadyValidated) Right(block) else blockValidation.validateBlockBeforeExecution(block)

    val blockExecResult = {
      if (block.hasCheckpoint) {
        // block with checkpoint is not executed normally - it's not need to do after execution validation
        preExecValidationResult.map(_ => Seq.empty[Receipt])
      } else {
        for {
          _ <- preExecValidationResult
          result <- executeBlock(block)
          _ <- blockValidation.validateBlockAfterExecution(
            block,
            result.worldState.stateRootHash,
            result.receipts,
            result.gasUsed
          )
        } yield result.receipts
      }
    }

    if (blockExecResult.isRight) {
      log.debug(s"Block ${block.header.number} (with hash: ${block.header.hashAsHexString}) executed correctly")
    }

    blockExecResult
  }

  /** Executes a block (executes transactions and pays rewards) */
  private def executeBlock(block: Block): Either[BlockExecutionError, BlockResult] = {
    for {
      parentHeader <- blockchain
        .getBlockHeaderByHash(block.header.parentHash)
        .toRight(MissingParentError) // Should not never occur because validated earlier
      initialWorld = InMemoryWorldStateProxy(
        evmCodeStorage = evmCodeStorage,
        blockchain.getBackingStorage(block.header.number),
        (number: BigInt) => blockchain.getBlockHeaderByNumber(number).map(_.hash),
        accountStartNonce = blockchainConfig.accountStartNonce,
        stateRootHash = parentHeader.stateRoot,
        noEmptyAccounts = EvmConfig.forBlock(parentHeader.number, blockchainConfig).noEmptyAccounts,
        ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
      )
      execResult <- executeBlockTransactions(block, initialWorld)
      worldToPersist <- Either
        .catchOnly[MPTException](blockPreparator.payBlockReward(block, execResult.worldState))
        .leftMap(BlockExecutionError.MPTError.apply)
      // State root hash needs to be up-to-date for validateBlockAfterExecution
      worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist)
    } yield execResult.copy(worldState = worldPersisted)
  }

  /** This function runs transactions
    *
    * @param block the block with transactions to run
    */
  protected[ledger] def executeBlockTransactions(
      block: Block,
      initialWorld: InMemoryWorldStateProxy
  ): Either[BlockExecutionError, BlockResult] = {
    val blockHeaderNumber = block.header.number
    executeBlockTransactions(block, blockHeaderNumber, initialWorld)
  }

  protected def executeBlockTransactions(
      block: Block,
      blockHeaderNumber: BigInt,
      initialWorld: InMemoryWorldStateProxy
  ): Either[BlockExecutionError.TxsExecutionError, BlockResult] = {
    val inputWorld = blockchainConfig.daoForkConfig match {
      case Some(daoForkConfig) if daoForkConfig.isDaoForkBlock(blockHeaderNumber) =>
        drainDaoForkAccounts(initialWorld, daoForkConfig)
      case _ => initialWorld
    }

    val hashAsHexString = block.header.hashAsHexString
    val transactionList = block.body.transactionList
    log.debug(
      s"About to execute ${transactionList.size} txs from block $blockHeaderNumber (with hash: $hashAsHexString)"
    )
    val blockTxsExecResult = blockPreparator.executeTransactions(transactionList, inputWorld, block.header)
    blockTxsExecResult match {
      case Right(_) => log.debug(s"All txs from block $hashAsHexString were executed successfully")
      case Left(error) =>
        log.debug(s"Not all txs from block $hashAsHexString were executed correctly, due to ${error.reason}")
    }
    blockTxsExecResult
  }

  /** This function updates worldState transferring balance from drainList accounts to refundContract address
    *
    * @param worldState     initial world state
    * @param daoForkConfig  dao fork configuration with drainList and refundContract config
    * @return updated world state proxy
    */
  private def drainDaoForkAccounts(
      worldState: InMemoryWorldStateProxy,
      daoForkConfig: DaoForkConfig
  ): InMemoryWorldStateProxy = {
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

  /** Executes and validates a list of blocks, storing the results in the blockchain.
    *
    * @param blocks   blocks to be executed
    * @param parentChainWeight parent weight
    *
    * @return a list of blocks in incremental order that were correctly executed and an optional [[BlockExecutionError]]
    */
  def executeAndValidateBlocks(
      blocks: List[Block],
      parentChainWeight: ChainWeight
  ): (List[BlockData], Option[BlockExecutionError]) = {
    @tailrec
    def go(
        executedBlocksDecOrder: List[BlockData],
        remainingBlocksIncOrder: List[Block],
        parentWeight: ChainWeight,
        error: Option[BlockExecutionError]
    ): (List[BlockData], Option[BlockExecutionError]) = {
      if (remainingBlocksIncOrder.isEmpty) {
        (executedBlocksDecOrder.reverse, None)
      } else {
        val blockToExecute = remainingBlocksIncOrder.head
        executeAndValidateBlock(blockToExecute, alreadyValidated = true) match {
          case Right(receipts) =>
            val newWeight = parentWeight.increase(blockToExecute.header)
            val newBlockData = BlockData(blockToExecute, receipts, newWeight)
            blockchain.save(newBlockData.block, newBlockData.receipts, newBlockData.weight, saveAsBestBlock = true)
            go(newBlockData :: executedBlocksDecOrder, remainingBlocksIncOrder.tail, newWeight, None)
          case Left(executionError) =>
            (executedBlocksDecOrder.reverse, Some(executionError))
        }
      }
    }

    go(List.empty[BlockData], blocks, parentChainWeight, None)
  }

}

sealed trait BlockExecutionError {
  val reason: Any
}

sealed trait BlockExecutionSuccess

final case object BlockExecutionSuccess extends BlockExecutionSuccess

object BlockExecutionError {
  final case class ValidationBeforeExecError(reason: Any) extends BlockExecutionError

  final case class StateBeforeFailure(worldState: InMemoryWorldStateProxy, acumGas: BigInt, acumReceipts: Seq[Receipt])

  final case class TxsExecutionError(stx: SignedTransaction, stateBeforeError: StateBeforeFailure, reason: String)
      extends BlockExecutionError

  final case class ValidationAfterExecError(reason: String) extends BlockExecutionError

  final case object MissingParentError extends BlockExecutionError {
    override val reason: Any = "Cannot find parent"
  }

  final case class MPTError(reason: MPTException) extends BlockExecutionError
}
