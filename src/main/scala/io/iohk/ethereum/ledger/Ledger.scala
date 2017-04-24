package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.validators._
import io.iohk.ethereum.ledger.BlockExecutionError.{TxsExecutionError, ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.Ledger.{PC, PR, TxResult, BlockResult}
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.validators.{BlockValidator, SignedTransactionValidator}
import io.iohk.ethereum.vm._
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec

trait Ledger {

  def executeBlock(block: Block, storages: BlockchainStorages, validators: Validators): Either[BlockExecutionError, Unit]

}

class LedgerImpl(vm: VM, blockchainConfig: BlockchainConfig) extends Ledger with Logger {

  def executeBlock(
    block: Block,
    storages: BlockchainStorages,
    validators: Validators): Either[BlockExecutionError, Unit] = {

    val blockchain = BlockchainImpl(storages)

    val blockExecResult = for {
      _ <- validateBlockBeforeExecution(block, blockchain, validators)

      execResult <- executeBlockTransactions(block, blockchain, storages, validators.signedTransactionValidator)
      BlockResult(resultingWorldStateProxy, gasUsed, receipts) = execResult
      worldToPersist = payBlockReward(blockchainConfig.blockReward, block, resultingWorldStateProxy)
      worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist) //State root hash needs to be up-to-date for validateBlockAfterExecution

      _ <- validateBlockAfterExecution(block, worldPersisted.stateRootHash, receipts, gasUsed, validators.blockValidator)
    } yield ()

    if(blockExecResult.isRight)
      log.debug(s"Block ${block.header.number} (with hash: ${block.header.hashAsHexString}) executed correctly")
    blockExecResult
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
    val initialWorld = InMemoryWorldStateProxy(storages, storages.nodeStorage, parentStateRoot)

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
  private def executeTransactions(signedTransactions: Seq[SignedTransaction], world: InMemoryWorldStateProxy,
                                  blockHeader: BlockHeader, signedTransactionValidator: SignedTransactionValidator,
                                  acumGas: BigInt = 0, acumReceipts: Seq[Receipt] = Nil): Either[TxsExecutionError, BlockResult] =
    signedTransactions match {
      case Nil =>
        Right(BlockResult(worldState = world, gasUsed = acumGas, receipts = acumReceipts))

      case Seq(stx, otherStxs@_*) =>
        val senderAccount = world.getAccount(stx.senderAddress)
        val validatedStx = senderAccount
          .toRight(Left(TxsExecutionError(s"Account of tx sender ${Hex.toHexString(stx.senderAddress.toArray)} not found")))
          .flatMap(account => signedTransactionValidator.validate(stx, account, blockHeader, calculateUpfrontCost, acumGas))
        validatedStx match {
          case Right(_) =>
            val TxResult(newWorld, gasUsed, logs) = executeTransaction(stx, blockHeader, world)

            val receipt = Receipt(
              postTransactionStateHash = newWorld.stateRootHash,
              cumulativeGasUsed = acumGas + gasUsed,
              logsBloomFilter = BloomFilter.create(logs),
              logs = logs
            )

            log.debug(s"Receipt generated for tx ${stx.hashAsHexString}, $receipt")

            executeTransactions(otherStxs, newWorld, blockHeader, signedTransactionValidator, receipt.cumulativeGasUsed, acumReceipts :+ receipt)
          case Left(error) => Left(TxsExecutionError(error.toString))
        }
    }

  private[ledger] def executeTransaction(stx: SignedTransaction, blockHeader: BlockHeader, world: InMemoryWorldStateProxy): TxResult = {
    log.debug(s"Transaction ${stx.hashAsHexString} execution start")
    val gasPrice = UInt256(stx.tx.gasPrice)
    val gasLimit = UInt256(stx.tx.gasLimit)
    val config = EvmConfig.forBlock(blockHeader.number, blockchainConfig)

    val checkpointWorldState = updateSenderAccountBeforeExecution(stx, world)
    val context = prepareProgramContext(stx, blockHeader, checkpointWorldState, config)
    val result = runVM(stx, context, config)

    val resultWithErrorHandling: PR =
      if(result.error.isDefined) {
        //Rollback to the world before transfer was done if an error happened
        result.copy(world = checkpointWorldState, addressesToDelete = Nil, logs = Nil)
      } else
        result

    val totalGasToRefund = calcTotalGasToRefund(stx, resultWithErrorHandling)
    val executionGasToPayToMiner = gasLimit - totalGasToRefund

    val refundGasFn = pay(stx.senderAddress, totalGasToRefund * gasPrice) _
    val payMinerForGasFn = pay(Address(blockHeader.beneficiary), executionGasToPayToMiner * gasPrice) _
    val deleteAccountsFn = deleteAccounts(resultWithErrorHandling.addressesToDelete) _
    val persistStateFn = InMemoryWorldStateProxy.persistState _

    val world2 = (refundGasFn andThen payMinerForGasFn andThen deleteAccountsFn andThen persistStateFn)(resultWithErrorHandling.world)

    log.debug(
      s"""Transaction ${stx.hashAsHexString} execution end. Summary:
         | - Error: ${result.error}.
         | - Total Gas to Refund: $totalGasToRefund
         | - Execution gas paid to miner: $executionGasToPayToMiner""".stripMargin)

    TxResult(world2, gasLimit - totalGasToRefund, resultWithErrorHandling.logs)
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
  private[ledger] def payBlockReward(minerRewardAmount: UInt256, block: Block, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {

    def getAccountToPay(address: Address, ws: InMemoryWorldStateProxy): Account = ws.getAccount(address).getOrElse(Account.Empty)

    // YP - eq 148
    def calcMinerReward(ommersCount: Int): UInt256 = minerRewardAmount + (minerRewardAmount * ommersCount) / 32

    // YP - eq 149
    def calcOmmerReward(blockHeader: BlockHeader, ommerBlockHeader: BlockHeader): UInt256 =
      minerRewardAmount - (minerRewardAmount * UInt256(blockHeader.number - ommerBlockHeader.number)) / 8

    val minerAddress = Address(block.header.beneficiary)
    val minerAccount = getAccountToPay(minerAddress, worldStateProxy)
    val minerReward = calcMinerReward(block.body.uncleNodesList.size)
    val afterMinerReward = worldStateProxy.saveAccount(minerAddress, minerAccount.increaseBalance(minerReward))
    log.debug(s"Paying block ${block.header.number} reward of $minerReward to miner with account address $minerAddress")

    block.body.uncleNodesList.foldLeft(afterMinerReward) { (ws, ommer) =>
      val ommerAddress = Address(ommer.beneficiary)
      val account = getAccountToPay(ommerAddress, ws)
      val ommerReward = calcOmmerReward(block.header, ommer)
      log.debug(s"Paying block ${block.header.number} reward of $ommerReward to ommer with account address $ommerAddress")
      ws.saveAccount(ommerAddress, account.increaseBalance(ommerReward))
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
        val address = worldStateProxy.createAddress(stx.senderAddress)
        val world1 = worldStateProxy.newEmptyAccount(address)
        val world2 = world1.transfer(stx.senderAddress, address, UInt256(stx.tx.value))
        ProgramContext(stx, address,  Program(stx.tx.payload), blockHeader, world2, config)

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
  private def calcTotalGasToRefund(stx: SignedTransaction, result: PR): UInt256 = {
    if (result.error.isDefined)
      0
    else {
      val gasUsed = UInt256(stx.tx.gasLimit) - result.gasRemaining
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
  private[ledger] def deleteAccounts(addressesToDelete: Seq[Address])(worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
    addressesToDelete.foldLeft(worldStateProxy){ case (world, address) => world.deleteAccount(address) }

}

object Ledger {
  type PC = ProgramContext[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PR = ProgramResult[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]

  case class BlockResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt = 0, receipts: Seq[Receipt] = Nil)
  case class TxResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt, logs: Seq[TxLogEntry])
}

trait BlockExecutionError

object BlockExecutionError {
  case class ValidationBeforeExecError(reason: String) extends BlockExecutionError
  case class TxsExecutionError(reason: String) extends BlockExecutionError
  case class ValidationAfterExecError(reason: String) extends BlockExecutionError
}
