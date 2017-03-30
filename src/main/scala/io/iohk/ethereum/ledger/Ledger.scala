package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.messages.PV63.Receipt
import io.iohk.ethereum.network.p2p.validators.BlockValidator
import io.iohk.ethereum.utils.{Config, Logger}
import io.iohk.ethereum.vm.{GasFee, _}

object Ledger extends Logger {

  type PR = ProgramResult[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type ExecResult = (InMemoryWorldStateProxy, BigInt)

  def executeBlock(
    block: Block,
    storages: BlockchainStorages,
    stateStorage: NodeStorage): Unit = {

    val blockError = validateBlockBeforeExecution(block)
    if (blockError.isEmpty) {
      log.debug(s"About to execute txs from block ${block.header}")
      val (resultingWorldStateProxy, gasUsed) = executeBlockTransactions(block, storages, stateStorage)
      log.debug(s"All txs from block ${block.header} were executed")

      val worldToPersist = payBlockReward(Config.Blockchain.BlockReward, block, resultingWorldStateProxy)
      val receipts = Seq() //FIXME: This should be obtained from block execution (done in EC-139)
      val afterExecutionBlockError = validateBlockAfterExecution(block, worldToPersist.stateRootHash, receipts, gasUsed)
      if (afterExecutionBlockError.isEmpty) {
        InMemoryWorldStateProxy.persistIfHashMatches(block.header.stateRoot, worldToPersist)
        log.debug(s"Block ${block.header} txs state changes persisted")
      } else throw new RuntimeException(afterExecutionBlockError.get)

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
    storages: BlockchainStorages,
    stateStorage: NodeStorage):
  ExecResult = {
    val blockchain = BlockchainImpl(storages)
    val initialWorldStateProxy = InMemoryWorldStateProxy(storages, stateStorage,
      blockchain.getBlockHeaderByHash(block.header.hash).map(_.stateRoot)
    )
    block.body.transactionList.foldLeft[ExecResult](initialWorldStateProxy -> 0) {
      case ((worldStateProxy, acumGas), stx) =>
        val result: Either[String, PR] = for {
          _ <- validateTransaction(stx, worldStateProxy, 0, block)
          worldStateProxy1 = updateAccountBeforeExecution(stx, worldStateProxy)
          result <- execute(stx, block.header, worldStateProxy1)
        } yield result

        result match {
          case Right(theResult) =>
            val gasUsed = stx.tx.gasLimit - theResult.gasRemaining
            val refundGasFn = {
              if (theResult.gasRemaining > 0) (refundGas _).curried(Address(block.header.beneficiary))(gasUsed)
              else identity[InMemoryWorldStateProxy] _
            }
            val payBeneficiariesFn = (payForGasUsedToBeneficiary _).curried(stx)
            val deleteAccountsFn = (deleteAccounts _).curried(theResult.addressesToDelete)
            (refundGasFn andThen payBeneficiariesFn andThen deleteAccountsFn) (theResult.world) -> (gasUsed + acumGas)
          case Left(error) => throw new RuntimeException(error)
        }
    }
  }

  private def validateBlockBeforeExecution(block: Block): Option[String] = None

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
    val afterMinerReward = worldStateProxy.saveAccount(minerAddress, minerAccount.updateBalance(minerReward))
    log.debug(s"Paying block ${block.header.number} reward of $minerReward to miner with account address $minerAddress")

    block.body.uncleNodesList.foldLeft(afterMinerReward) { (ws, ommer) =>
      val ommerAddress = Address(ommer.beneficiary)
      val account = getAccountToPay(ommerAddress, ws)
      val ommerReward = calcOmmerReward(block.header, ommer)
      log.debug(s"Paying block ${ommer.number} reward of $ommerReward to ommer with account address $ommerAddress")
      ws.saveAccount(ommerAddress, account.updateBalance(ommerReward))
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
  private def calculateUpfrontCost(tx: Transaction): BigInt = calculateUpfrontGas(tx) + tx.value

  /**
    * Initial tests of intrinsic validity stated in Section 6 of YP
    *
    * @param stx           Transaction to validate
    * @param accumGasLimit Total amount of gas spent prior this transaction within the container block
    * @param block         Container block
    * @return Transaction if valid, error otherwise
    */
  private def validateTransaction(
    stx: SignedTransaction,
    worldState: InMemoryWorldStateProxy,
    accumGasLimit: BigInt,
    block: Block): Either[String, SignedTransaction] = {
    for {
      _ <- checkSyntacticValidity(stx)
      _ <- validateSignature(stx)
      _ <- validateNonce(stx, worldState)
      _ <- validateGas(stx, block.header)
      _ <- validateAccountHasEnoughGasToPayUpfrontCost(stx, worldState)
      _ <- validateGasLimit(stx, accumGasLimit, block.header.gasLimit)
    } yield stx
  }

  private def checkSyntacticValidity(stx: SignedTransaction): Either[String, SignedTransaction] =
    if (stx.syntacticValidity) Right(stx) else Left("Transaction is not well formed")

  /**
    * Validates if the transaction signature is valid
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or an error description
    */
  private def validateSignature(stx: SignedTransaction): Either[String, SignedTransaction] = Right(stx) //TODO

  /**
    * Validates if the transaction nonce matches current sender account's nonce
    *
    * @param stx Transaction to validate
    * @return Either the validated transaction or an error description
    */
  private def validateNonce(stx: SignedTransaction, worldStateProxy: InMemoryWorldStateProxy): Either[String, SignedTransaction] = {
    if (worldStateProxy.getGuaranteedAccount(stx.recoveredSenderAddress.get).nonce == stx.tx.nonce) Right(stx)
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
    val accountBalance = worldStateProxy.getGuaranteedAccount(stx.recoveredSenderAddress.get).balance
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
  private def updateAccountBeforeExecution(stx: SignedTransaction, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    val senderAddress = stx.recoveredSenderAddress.get
    val account = worldStateProxy.getGuaranteedAccount(senderAddress)
    worldStateProxy.saveAccount(senderAddress, account.copy(
      nonce = account.nonce + 1,
      balance = account.balance - UInt256(calculateUpfrontGas(stx.tx))
    ))
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

  private def execute(stx: SignedTransaction, blockHeader: BlockHeader, worldStateProxy: InMemoryWorldStateProxy): Either[String, PR] = {
    val programContext = ProgramContext[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage](stx, blockHeader, worldStateProxy)
    val result = VM.run(programContext)
    if (result.error.isDefined) Left(result.error.get.toString)
    else {
      if (stx.tx.isContractInit) payContractCreationCost(result).map(saveCreatedCost(programContext.env.ownerAddr, _))
      else Right(result)
    }
  }

  private def payContractCreationCost(result: PR): Either[String, PR] = {
    val codeDepositCost = GasFee.calcCodeDepositCost(result.returnData)
    if (result.gasRemaining < codeDepositCost) Left(OutOfGas.toString)
    else Right(result.copy(gasRemaining = result.gasRemaining - UInt256(codeDepositCost)))
  }

  private def saveCreatedCost(ownerAddress: Address, result: PR): PR = {
    result.copy(
      world = result.world.saveCode(ownerAddress, result.returnData)
    )
  }

  private def refundGas(address: Address, gasUsed: BigInt, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = worldStateProxy //TODO

  /**
    * The Ether for the gas is given to the miner, whose address is specified as the beneficiary of the container block
    * See eq (75) of YP
    *
    * @param stx
    * @param worldStateProxy
    * @return
    */
  private def payForGasUsedToBeneficiary(stx: SignedTransaction, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = worldStateProxy //TODO

  /**
    * Delete all accounts (that appear in SUICIDE list). YP eq (78)
    *
    * @param addressesToDelete
    * @param worldStateProxy
    * @return
    */
  private def deleteAccounts(addressesToDelete: Seq[Address], worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = worldStateProxy //TODO

}
