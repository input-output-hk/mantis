package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.validators._
import io.iohk.ethereum.ledger.BlockExecutionError.{TxsExecutionError, ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.Ledger.{BlockResult, PC, PR, TxResult, BlockPreparationResult}
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.validators.{BlockValidator, SignedTransactionValidator}
import io.iohk.ethereum.vm.UInt256._
import io.iohk.ethereum.vm._
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec

trait Ledger {

  def executeBlock(block: Block, storages: BlockchainStorages, validators: Validators): Either[BlockExecutionError, Seq[Receipt]]

  def prepareBlock(block: Block, storages: BlockchainStorages, validators: Validators): Either[BlockPreparationError, BlockPreparationResult]

  def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader, storages: BlockchainStorages): TxResult
}

class LedgerImpl(vm: VM, blockchainConfig: BlockchainConfig) extends Ledger with Logger {

  val blockRewardCalculator = new BlockRewardCalculator(blockchainConfig.monetaryPolicyConfig)

  def executeBlock(
    block: Block,
    storages: BlockchainStorages,
    validators: Validators): Either[BlockExecutionError, Seq[Receipt]] = {

    val blockchain = BlockchainImpl(storages)

    val blockExecResult = for {
      _ <- validateBlockBeforeExecution(block, blockchain, validators)

      execResult <- executeBlockTransactions(block, blockchain, storages, validators.signedTransactionValidator)
      BlockResult(resultingWorldStateProxy, gasUsed, receipts) = execResult
      worldToPersist = payBlockReward(block, resultingWorldStateProxy)
      worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist) //State root hash needs to be up-to-date for validateBlockAfterExecution

      _ <- validateBlockAfterExecution(block, worldPersisted.stateRootHash, receipts, gasUsed, validators.blockValidator)
    } yield receipts

    if(blockExecResult.isRight)
      log.debug(s"Block ${block.header.number} (with hash: ${block.header.hashAsHexString}) executed correctly")
    blockExecResult
  }

  def prepareBlock(
    block: Block,
    storages: BlockchainStorages,
    validators: Validators): Either[BlockPreparationError, BlockPreparationResult] = {

    val blockchain = BlockchainImpl(storages)

    val blockExecResult = for {
      //todo why do we require in executeBlockTransactions separate blockchain and blockchain storage
      //todo blockchain storage is part of blockchain
      execResult <- executeBlockTransactions(block, blockchain, storages, validators.signedTransactionValidator)
      BlockResult(resultingWorldStateProxy, _, _) = execResult
      worldToPersist = payBlockReward(block, resultingWorldStateProxy)
      worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist) //State root hash needs to be up-to-date for prepared block
    } yield BlockPreparationResult(block, execResult, worldPersisted.stateRootHash)

    blockExecResult match {
      case Right(result) =>
        log.debug(s"Prepared for mining block with number ${block.header.number}")
        Right(result)
      case Left(TxsExecutionError(tx, reason)) =>
        log.debug(s"failed to execute transaction $tx because: $reason")
        val newBlock = block.copy(body = block.body.copy(transactionList = block.body.transactionList.filter(_.hash != tx.hash)))
        prepareBlock(newBlock, storages, validators)
      case Left(error: BlockExecutionError) =>
        Left(TxError(error.reason))
    }
  }

  /**
    * This function runs transaction
    *
    * @param block
    * @param blockchain
    * @param storages
    * @param signedTransactionValidator
    */
  private[ledger] def executeBlockTransactions(
    block: Block,
    blockchain: Blockchain,
    storages: BlockchainStorages,
    signedTransactionValidator: SignedTransactionValidator):
  Either[BlockExecutionError, BlockResult] = {
    val parentStateRoot = blockchain.getBlockHeaderByHash(block.header.parentHash).map(_.stateRoot)
    val initialWorld = InMemoryWorldStateProxy(storages, parentStateRoot)

    log.debug(s"About to execute ${block.body.transactionList.size} txs from block ${block.header.number} (with hash: ${block.header.hashAsHexString})")
    val blockTxsExecResult = executeTransactions(block.body.transactionList, initialWorld, block.header, signedTransactionValidator)
    blockTxsExecResult match {
      case Right(_) => log.debug(s"All txs from block ${block.header.hashAsHexString} were executed successfully")
      case Left(error) => log.debug(s"Not all txs from block ${block.header.hashAsHexString} were executed correctly, due to ${error.reason}")
    }
    blockTxsExecResult
  }

  /**
    * This functions executes all the signed transactions from a block (till one of those executions fails)
    *
    * @param signedTransactions from the block that are left to execute
    * @param world that will be updated by the execution of the signedTransactions
    * @param blockHeader of the block we are currently executing
    * @param signedTransactionValidator used to validate the signed transactions
    * @param acumGas, accumulated gas of the previoulsy executed transactions of the same block
    * @param acumReceipts, accumulated receipts of the previoulsy executed transactions of the same block
    * @return a BlockResult if the execution of all the transactions in the block was successful or a BlockExecutionError
    *         if one of them failed
    */
  @tailrec
  private[ledger] final def executeTransactions(signedTransactions: Seq[SignedTransaction], world: InMemoryWorldStateProxy,
                                  blockHeader: BlockHeader, signedTransactionValidator: SignedTransactionValidator,
                                  acumGas: BigInt = 0, acumReceipts: Seq[Receipt] = Nil): Either[TxsExecutionError, BlockResult] =
    signedTransactions match {
      case Nil =>
        Right(BlockResult(worldState = world, gasUsed = acumGas, receipts = acumReceipts))

      case Seq(stx, otherStxs@_*) =>
        val (senderAccount, worldForTx) = world.getAccount(stx.senderAddress).map(a => (a, world))
          .getOrElse ((Account.Empty, world.saveAccount(stx.senderAddress, Account.Empty)))
        val upfrontCost = calculateUpfrontCost(stx.tx)
        val validatedStx = signedTransactionValidator.validate(stx, senderAccount, blockHeader, upfrontCost, acumGas)

        validatedStx match {
          case Right(_) =>
            val TxResult(newWorld, gasUsed, logs, _) = executeTransaction(stx, blockHeader, worldForTx)

            val receipt = Receipt(
              postTransactionStateHash = newWorld.stateRootHash,
              cumulativeGasUsed = acumGas + gasUsed,
              logsBloomFilter = BloomFilter.create(logs),
              logs = logs
            )

            log.debug(s"Receipt generated for tx ${stx.hashAsHexString}, $receipt")

            executeTransactions(otherStxs, newWorld, blockHeader, signedTransactionValidator, receipt.cumulativeGasUsed, acumReceipts :+ receipt)
          case Left(error) => Left(TxsExecutionError(stx, error.toString))
        }
  }

  override def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader, storages: BlockchainStorages): TxResult = {
    val stateRoot = blockHeader.stateRoot

    val gasLimit = stx.tx.gasLimit
    val config = EvmConfig.forBlock(blockHeader.number, blockchainConfig)

    val world1 = InMemoryWorldStateProxy(storages, Some(stateRoot))
    val world2 =
      if (world1.getAccount(stx.senderAddress).isEmpty)
        world1.saveAccount(stx.senderAddress, Account.Empty)
      else
        world1

    val worldForTx = updateSenderAccountBeforeExecution(stx, world2)
    val context: PC = prepareProgramContext(stx, blockHeader, worldForTx, config)

    val result = runVM(stx, context, config)

    val totalGasToRefund = calcTotalGasToRefund(stx, result)

    TxResult(result.world, gasLimit - totalGasToRefund, result.logs, result.returnData)
  }

  private[ledger] def executeTransaction(stx: SignedTransaction, blockHeader: BlockHeader, world: InMemoryWorldStateProxy): TxResult = {
    log.debug(s"Transaction ${stx.hashAsHexString} execution start")
    val gasPrice = UInt256(stx.tx.gasPrice)
    val gasLimit = stx.tx.gasLimit
    val config = EvmConfig.forBlock(blockHeader.number, blockchainConfig)

    val checkpointWorldState = updateSenderAccountBeforeExecution(stx, world)
    val context = prepareProgramContext(stx, blockHeader, checkpointWorldState, config)
    val result = runVM(stx, context, config)

    val resultWithErrorHandling: PR =
      if(result.error.isDefined) {
        //Rollback to the world before transfer was done if an error happened
        result.copy(world = checkpointWorldState, addressesToDelete = Set.empty, logs = Nil)
      } else
        result

    val totalGasToRefund = calcTotalGasToRefund(stx, resultWithErrorHandling)
    val executionGasToPayToMiner = gasLimit - totalGasToRefund

    val refundGasFn = pay(stx.senderAddress, (totalGasToRefund * gasPrice).toUInt256) _
    val payMinerForGasFn = pay(Address(blockHeader.beneficiary), (executionGasToPayToMiner * gasPrice).toUInt256) _
    val deleteAccountsFn = deleteAccounts(resultWithErrorHandling.addressesToDelete) _
    val persistStateFn = InMemoryWorldStateProxy.persistState _

    val world2 = (refundGasFn andThen payMinerForGasFn andThen deleteAccountsFn andThen persistStateFn)(resultWithErrorHandling.world)

    log.debug(
      s"""Transaction ${stx.hashAsHexString} execution end. Summary:
         | - Error: ${result.error}.
         | - Total Gas to Refund: $totalGasToRefund
         | - Execution gas paid to miner: $executionGasToPayToMiner""".stripMargin)

    TxResult(world2, gasLimit - totalGasToRefund, resultWithErrorHandling.logs, result.returnData)
  }

  private def validateBlockBeforeExecution(block: Block, blockchain: Blockchain, validators: Validators): Either[BlockExecutionError, Unit] = {
    val result = for {
      _ <- validators.blockHeaderValidator.validate(block.header, blockchain)
      _ <- validators.blockValidator.validateHeaderAndBody(block.header, block.body)
      _ <- validators.ommersValidator.validate(block.header.number, block.body.uncleNodesList, blockchain)
    } yield ()
    result.left.map(error => ValidationBeforeExecError(error.toString))
  }

  /**
    * This function validates that the various results from execution are consistent with the block. This includes:
    *   - Validating the resulting stateRootHash
    *   - Doing BlockValidator.validateBlockReceipts validations involving the receipts
    *   - Validating the resulting gas used
    *
    * @param block to validate
    * @param stateRootHash from the resulting state trie after executing the txs from the block
    * @param receipts associated with the execution of each of the tx from the block
    * @param gasUsed, accumulated gas used for the execution of the txs from the block
    * @param blockValidator used to validate the receipts with the block
    * @return None if valid else a message with what went wrong
    */
  private[ledger] def validateBlockAfterExecution(block: Block, stateRootHash: ByteString, receipts: Seq[Receipt],
                                                  gasUsed: BigInt, blockValidator: BlockValidator): Either[BlockExecutionError, Unit] = {
    lazy val blockAndReceiptsValidation = blockValidator.validateBlockAndReceipts(block, receipts)
    if(block.header.gasUsed != gasUsed)
      Left(ValidationAfterExecError(s"Block has invalid gas used, expected ${block.header.gasUsed} but got $gasUsed"))
    else if(block.header.stateRoot != stateRootHash)
      Left(ValidationAfterExecError(
        s"Block has invalid state root hash, expected ${Hex.toHexString(block.header.stateRoot.toArray)} but got ${Hex.toHexString(stateRootHash.toArray)}")
      )
    else if(blockAndReceiptsValidation.isLeft)
      Left(ValidationAfterExecError(blockAndReceiptsValidation.left.get.toString))
    else
      Right(())
  }

  /**
    * This function updates state in order to pay rewards based on YP section 11.3
    *
    * @param block
    * @param worldStateProxy
    * @return
    */
  private[ledger] def payBlockReward(block: Block, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {

    def getAccountToPay(address: Address, ws: InMemoryWorldStateProxy): Account = ws.getAccount(address).getOrElse(Account.Empty)

    val minerAddress = Address(block.header.beneficiary)
    val minerAccount = getAccountToPay(minerAddress, worldStateProxy)
    val minerReward = blockRewardCalculator.calcBlockMinerReward(block.header.number, block.body.uncleNodesList.size)
    val afterMinerReward = worldStateProxy.saveAccount(minerAddress, minerAccount.increaseBalance(UInt256(minerReward)))
    log.debug(s"Paying block ${block.header.number} reward of $minerReward to miner with account address $minerAddress")

    block.body.uncleNodesList.foldLeft(afterMinerReward) { (ws, ommer) =>
      val ommerAddress = Address(ommer.beneficiary)
      val account = getAccountToPay(ommerAddress, ws)
      val ommerReward = blockRewardCalculator.calcOmmerMinerReward(block.header.number, ommer.number)
      log.debug(s"Paying block ${block.header.number} reward of $ommerReward to ommer with account address $ommerAddress")
      ws.saveAccount(ommerAddress, account.increaseBalance(UInt256(ommerReward)))
    }
  }

  /**
    * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price). See YP equation number (68)
    *
    * @param tx Target transaction
    * @return Upfront cost
    */
  private def calculateUpfrontGas(tx: Transaction): UInt256 = UInt256(tx.gasLimit * tx.gasPrice)

  /**
    * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price) + Tv (Tx value). See YP equation number (65)
    *
    * @param tx Target transaction
    * @return Upfront cost
    */
  private[ledger] def calculateUpfrontCost(tx: Transaction): UInt256 =
    UInt256(calculateUpfrontGas(tx) + tx.value)

  /**
    * Increments account nonce by 1 stated in YP equation (69) and
    * Pays the upfront Tx gas calculated as TxGasPrice * TxGasLimit from balance. YP equation (68)
    *
    * @param stx
    * @param worldStateProxy
    * @return
    */
  private[ledger] def updateSenderAccountBeforeExecution(stx: SignedTransaction, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    val senderAddress = stx.senderAddress
    val account = worldStateProxy.getGuaranteedAccount(senderAddress)
    worldStateProxy.saveAccount(senderAddress, account.increaseBalance(-calculateUpfrontGas(stx.tx)).increaseNonce)
  }

  private[ledger] def prepareProgramContext(stx: SignedTransaction, blockHeader: BlockHeader, worldStateProxy: InMemoryWorldStateProxy, config: EvmConfig): PC =
    stx.tx.receivingAddress match {
      case None =>
        val address = worldStateProxy.createAddress(creatorAddr = stx.senderAddress)
        val world1 = worldStateProxy.transfer(stx.senderAddress, address, UInt256(stx.tx.value))
        ProgramContext(stx, address,  Program(stx.tx.payload), blockHeader, world1, config)

      case Some(txReceivingAddress) =>
        val world1 = worldStateProxy.transfer(stx.senderAddress, txReceivingAddress, UInt256(stx.tx.value))
        ProgramContext(stx, txReceivingAddress, Program(world1.getCode(txReceivingAddress)), blockHeader, world1, config)
    }

  private def runVM(stx: SignedTransaction, context: PC, config: EvmConfig): PR = {
    val result: PR = vm.run(context)
    if (stx.tx.isContractInit && result.error.isEmpty)
      saveNewContract(context.env.ownerAddr, result, config)
    else
      result
  }

  private def saveNewContract(address: Address, result: PR, config: EvmConfig): PR = {
    val codeDepositCost = config.calcCodeDepositCost(result.returnData)
    if (result.gasRemaining < codeDepositCost) {
      if (config.exceptionalFailedCodeDeposit)
        result.copy(error = Some(OutOfGas))
      else
        result
    } else {
      result.copy(
        gasRemaining = result.gasRemaining - codeDepositCost,
        world = result.world.saveCode(address, result.returnData))
    }
  }

  /**
    * Calculate total gas to be refunded
    * See YP, eq (72)
    */
  private def calcTotalGasToRefund(stx: SignedTransaction, result: PR): BigInt = {
    if (result.error.isDefined)
      0
    else {
      val gasUsed = stx.tx.gasLimit - result.gasRemaining
      result.gasRemaining + (gasUsed / 2).min(result.gasRefund)
    }
  }

  private def pay(address: Address, value: UInt256)(world: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    val account = world.getAccount(address).getOrElse(Account.Empty).increaseBalance(value)
    world.saveAccount(address, account)
  }

  /**
    * Delete all accounts (that appear in SUICIDE list). YP eq (78).
    * The contract storage should be cleared during pruning as nodes could be used in other tries.
    * The contract code is also not deleted as there can be contracts with the exact same code, making it risky to delete
    * the code of an account in case it is shared with another one.
    * FIXME: Should we keep track of this for deletion? Maybe during pruning we can also prune contract code.
    *
    * @param addressesToDelete
    * @param worldStateProxy
    * @return a worldState equal worldStateProxy except that the accounts from addressesToDelete are deleted
    */
  private[ledger] def deleteAccounts(addressesToDelete: Set[Address])(worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
    addressesToDelete.foldLeft(worldStateProxy){ case (world, address) => world.deleteAccount(address) }

}

object Ledger {
  type PC = ProgramContext[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PR = ProgramResult[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]

  case class BlockResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt = 0, receipts: Seq[Receipt] = Nil)
  case class BlockPreparationResult(block: Block, blockResult: BlockResult, stateRootHash: ByteString)
  case class TxResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt, logs: Seq[TxLogEntry], vmReturnData: ByteString)
}

sealed trait BlockExecutionError{
  val reason: String
}

object BlockExecutionError {
  case class ValidationBeforeExecError(reason: String) extends BlockExecutionError
  case class TxsExecutionError(stx: SignedTransaction, reason: String) extends BlockExecutionError
  case class ValidationAfterExecError(reason: String) extends BlockExecutionError
}

trait BlockPreparationError

case class TxError(reason: String) extends BlockPreparationError
