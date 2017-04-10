package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.validators.{BlockHeaderValidator, BlockValidator, OmmersValidator, SignedTransactionValidator}
import io.iohk.ethereum.utils.{Config, Logger}
import io.iohk.ethereum.vm.{GasFee, _}
import org.spongycastle.util.encoders.Hex

class Ledger(vm: VM) extends Logger {

  type PC = ProgramContext[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PR = ProgramResult[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  case class BlockResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt = 0, receipts: Seq[Receipt] = Nil)
  case class TxResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt, logs: Seq[TxLogEntry])

  def executeBlock(
    block: Block,
    storages: BlockchainStorages,
    stateStorage: NodeStorage): Unit = {

    val blockchain = BlockchainImpl(storages)
    val blockError = validateBlockBeforeExecution(block, blockchain)
    if (blockError.isEmpty) {
      log.debug(s"About to execute txs from block ${block.header}")
      val BlockResult(resultingWorldStateProxy, gasUsed, receipts) = executeBlockTransactions(block, blockchain, storages, stateStorage)
      log.debug(s"All txs from block ${block.header} were executed")

      val worldToPersist = payBlockReward(Config.Blockchain.BlockReward, block, resultingWorldStateProxy)
      val worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist) //State root hash needs to be up-to-date for validateBlockAfterExecution

      val afterExecutionBlockError = validateBlockAfterExecution(block, worldPersisted.stateRootHash, receipts, gasUsed)
      if (afterExecutionBlockError.isEmpty)
        log.debug(s"Block ${Hex.toHexString(block.header.hash.toArray)} executed correctly")
      else throw new RuntimeException(afterExecutionBlockError.get)

    } else throw new RuntimeException(blockError.get)
  }

  /**
    * This function runs transaction
    *
    * @param block
    * @param storages
    * @param stateStorage
    */
  private def executeBlockTransactions(
    block: Block,
    blockchain: Blockchain,
    storages: BlockchainStorages,
    stateStorage: NodeStorage):
  BlockResult = {
    val parentStateRoot = blockchain.getBlockHeaderByHash(block.header.parentHash).map(_.stateRoot)
    val initialWorld = InMemoryWorldStateProxy(storages, stateStorage, parentStateRoot)

    block.body.transactionList.foldLeft[BlockResult](BlockResult(worldState = initialWorld)) {
      case (BlockResult(world, acumGas, receipts), stx)=>
        validateTransaction(stx, world, acumGas, block.header) match {
          case Left(err) =>
            throw new RuntimeException(err)

          case Right(_) =>
            val TxResult(newWorld, gasUsed, logs) = executeTransaction(stx, block.header, world)

            val receipt = Receipt(
              postTransactionStateHash = newWorld.stateRootHash,
              cumulativeGasUsed = acumGas + gasUsed,
              logsBloomFilter = BloomFilter.create(logs),
              logs = logs
            )

            BlockResult(newWorld, receipt.cumulativeGasUsed, receipts :+ receipt)
        }
    }
  }

  private[ledger] def executeTransaction(stx: SignedTransaction, blockHeader: BlockHeader, world: InMemoryWorldStateProxy): TxResult = {
    val gasPrice = UInt256(stx.tx.gasPrice)
    val gasLimit = UInt256(stx.tx.gasLimit)

    val world1 = updateSenderAccountBeforeExecution(stx, world)
    val result = runVM(stx, blockHeader, world1)

    val gasUsed = if(result.error.isDefined) gasLimit else gasLimit - result.gasRemaining
    val gasRefund = calcGasRefund(stx, result)

    val refundGasFn = pay(stx.senderAddress, gasRefund * gasPrice) _
    val payMinerForGasFn = pay(Address(blockHeader.beneficiary), (gasLimit - gasRefund) * gasPrice) _
    val deleteAccountsFn = deleteAccounts(result.addressesToDelete) _
    val persistStateFn = InMemoryWorldStateProxy.persistState _

    val world2 = (refundGasFn andThen payMinerForGasFn andThen deleteAccountsFn andThen persistStateFn)(result.world)

    TxResult(world2, gasUsed, result.logs)
  }

  private def validateBlockBeforeExecution(block: Block, blockchain: Blockchain): Option[String] = {
    val result = for {
      _ <- BlockHeaderValidator.validate(block.header, blockchain)
      _ <- BlockValidator.validateHeaderAndBody(block.header, block.body)
      _ <- OmmersValidator.validate(block.header.number, block.body.uncleNodesList, blockchain)
    } yield block
    result.swap.toOption.map(_.toString)
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
    * @return None if valid else a message with what went wrong
    */
  private[ledger] def validateBlockAfterExecution(block: Block, stateRootHash: ByteString,
                                                  receipts: Seq[Receipt], gasUsed: BigInt): Option[String] = {
    lazy val blockAndReceiptsValidation = BlockValidator.validateBlockAndReceipts(block, receipts)
    if(block.header.gasUsed != gasUsed)
      Some(s"Block has invalid gas used: ${block.header.gasUsed} != $gasUsed")
    else if(block.header.stateRoot != stateRootHash)
      Some(s"Block has invalid state root hash: ${block.header.stateRoot} != $stateRootHash")
    else if(blockAndReceiptsValidation.isLeft)
      Some(blockAndReceiptsValidation.left.get.toString)
    else
      None
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
      log.debug(s"Paying block ${ommer.number} reward of $ommerReward to ommer with account address $ommerAddress")
      ws.saveAccount(ommerAddress, account.increaseBalance(ommerReward))
    }
  }

  /**
    * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price). See YP equation number (68)
    *
    * @param tx Target transaction
    * @return Upfront cost
    */
  private def calculateUpfrontGas(tx: Transaction): BigInt = tx.gasLimit * tx.gasPrice

  /**
    * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price) + Tv (Tx value). See YP equation number (65)
    *
    * @param tx Target transaction
    * @return Upfront cost
    */
  private[ledger] def calculateUpfrontCost(tx: Transaction): UInt256 =
    UInt256(calculateUpfrontGas(tx) + tx.value)

  /**
    * Initial tests of intrinsic validity stated in Section 6 of YP
    *
    * @param stx           Transaction to validate
    * @param accumGasLimit Total amount of gas spent prior this transaction within the container block
    * @param blockHeader   Container block header
    * @return Transaction if valid, error otherwise
    */
  private def validateTransaction(
    stx: SignedTransaction,
    worldState: InMemoryWorldStateProxy,
    accumGasLimit: BigInt,
    blockHeader: BlockHeader): Either[String, SignedTransaction] = {
    for {
      _ <- SignedTransactionValidator.validateTransaction(stx, fromBeforeHomestead = blockHeader.number < Config.Blockchain.HomesteadBlock)
        .left.map(_.toString)
      _ <- validateNonce(stx, worldState)
      _ <- validateGas(stx, blockHeader)
      _ <- validateAccountHasEnoughGasToPayUpfrontCost(stx, worldState)
      _ <- validateGasLimit(stx, accumGasLimit, blockHeader.gasLimit)
    } yield stx
  }

  /**
    * Validates if the transaction nonce matches current sender account's nonce
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or an error description
    */
  private def validateNonce(stx: SignedTransaction, worldStateProxy: InMemoryWorldStateProxy): Either[String, SignedTransaction] = {
    if (worldStateProxy.getAccount(stx.senderAddress).map(_.nonce).contains(stx.tx.nonce)) Right(stx)
    else Left("Account nonce is different from TX sender nonce")
  }

  /**
    * Validates the gas limit is no smaller than the intrinsic gas used by the transaction.
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or an error description
    */
  private def validateGas(stx: SignedTransaction, blockHeader: BlockHeader): Either[String, SignedTransaction] = {
    import stx.tx
    if (stx.tx.gasLimit >= GasFee.calcTransactionIntrinsicGas(tx.payload, tx.isContractInit, blockHeader.number)) Right(stx)
    else Left("Transaction gas limit is less than the transaction execution gast (intrinsic gas)")
  }

  /**
    * Validates the sender account balance contains at least the cost required in up-front payment.
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or an error description
    */
  private def validateAccountHasEnoughGasToPayUpfrontCost(stx: SignedTransaction, worldStateProxy: InMemoryWorldStateProxy):
  Either[String, SignedTransaction] = {
    val accountBalance = worldStateProxy.getGuaranteedAccount(stx.senderAddress).balance
    val upfrontCost = calculateUpfrontCost(stx.tx)
    if (accountBalance >= upfrontCost) Right(stx)
    else Left(s"Sender account doesn't have enough balance to pay upfront cost $upfrontCost > $accountBalance")
  }


  /**
    * Increments account nonce by 1 stated in YP equation (69) and
    * Pays the upfront Tx gas calculated as TxGasPrice * TxGasLimit + TxValue from balance. YP equation (68)
    *
    * @param stx
    * @param worldStateProxy
    * @return
    */
  private[ledger] def updateSenderAccountBeforeExecution(stx: SignedTransaction, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    val senderAddress = stx.senderAddress
    val account = worldStateProxy.getGuaranteedAccount(senderAddress)
    worldStateProxy.saveAccount(senderAddress, account.increaseBalance(-calculateUpfrontCost(stx.tx)).increaseNonce)
  }

  /**
    * The sum of the transaction’s gas limit and the gas utilised in this block prior must be no greater than the
    * block’s gasLimit
    *
    * @param stx           Transaction to validate
    * @param accumGasLimit Gas spent within tx container block prior executing stx
    * @param blockGasLimit Block gas limit
    * @return Either the validated transaction or an error description
    */
  def validateGasLimit(stx: SignedTransaction, accumGasLimit: BigInt, blockGasLimit: BigInt): Either[String, SignedTransaction] = {
    if (stx.tx.gasLimit + accumGasLimit <= blockGasLimit) Right(stx)
    else Left("Transaction gas limit plus accumulated gas exceeds block gas limit")
  }

  private def runVM(stx: SignedTransaction, blockHeader: BlockHeader, worldStateProxy: InMemoryWorldStateProxy): PR = {
    val context: PC = ProgramContext(stx, blockHeader, worldStateProxy)
    val result = vm.run(context)
    if (stx.tx.isContractInit && result.error.isEmpty)
      saveNewContract(context.env.ownerAddr, result)
    else
      result
  }

  private def saveNewContract(address: Address, result: PR): PR = {
    val codeDepositCost = GasFee.calcCodeDepositCost(result.returnData)
    if (result.gasRemaining < codeDepositCost)
      result.copy(error = Some(OutOfGas))
    else
      result.copy(
        gasRemaining = result.gasRemaining - codeDepositCost,
        world = result.world.saveCode(address, result.returnData)
      )
  }

  /**
    * Calculate gas refund
    * See YP, eq (72) - only the right addend
    */
  private def calcGasRefund(stx: SignedTransaction, result: PR): UInt256 = {
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
    * Delete all accounts (that appear in SUICIDE list). YP eq (78)
    *
    * @param addressesToDelete
    * @param worldStateProxy
    * @return
    */
  private def deleteAccounts(addressesToDelete: Seq[Address])(worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = worldStateProxy //TODO

}

object Ledger extends Ledger(VM)
