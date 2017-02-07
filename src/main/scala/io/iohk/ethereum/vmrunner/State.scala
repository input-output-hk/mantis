package io.iohk.ethereum.vmrunner

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.vm._
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.RLPImplicits._

import scala.collection.mutable

object State extends AccountRetriever {
  private object ContractCreator {
    val address = Address(DataWord(0xabcdef))
    var nonce: BigInt = 0

    def newAddress(): Address = {
      nonce += 1
      val hash = sha3(rlp.encode(RLPList(address.bytes, nonce)))
      Address(DataWord(hash)) // FIXME: meh
    }

  }

  private var unnamedPrefix = 1

  private val accountNames = mutable.Map[String, Address]()
  private val accounts = mutable.Map[Address, XAccount]()
  private val programs = mutable.Map[ByteString, Program]()
  private val storages = mutable.Map[ByteString, Storage]()

  def listAccounts(): List[XAccount] =
    accounts.values.toList

  def getXAccount(name: String): Option[XAccount] =
    accountNames.get(name).flatMap(accounts.get)

  def getXAccount(address: Address): Option[XAccount] =
    accounts.get(address)

  def getAccount(address: Address): Option[Account] =
    accounts.get(address).map(_.acc)

  def getProgram(account: Account): Program =
    programs.getOrElse(account.codeHash, Program(ByteString.empty))

  def getStorage(account: Account): Storage =
    storages.getOrElse(account.storageRoot, Storage.Empty)

  def createAccount(name: String, balance: BigInt, gas: BigInt, code: ByteString, abis: Seq[ABI]): (Address, ProgramResult) = {
    val address = ContractCreator.newAddress()
    val initAccount = Account(0, balance, address.bytes, ByteString.empty) // TODO: what should the initial storage root really be?
    val initXAccount = XAccount(initAccount, name, address, abis)
    accounts += address -> initXAccount

    val tx = MockVmInput.transaction(ContractCreator.address, code, balance, gas)
    val bh = MockVmInput.blockHeader

    val context = ProgramContext(tx, bh, this)
    val intermediateResult = VM.run(context)

    val result = if (intermediateResult.error.isDefined) intermediateResult else {
      val depositCost = GasFee.G_codedeposit * intermediateResult.returnData.size
      intermediateResult.copy(
        gasRemaining = intermediateResult.gasRemaining - depositCost,
        gasUsed = intermediateResult.gasUsed + depositCost,
        error = if (intermediateResult.gasRemaining < depositCost) Some(OutOfGas) else None
      )
    }

    if (result.error.isEmpty) {
      val codeHash = ByteString(sha3(result.returnData.toArray))
      val account = initAccount.copy(codeHash = codeHash)
      val xAccount = initXAccount.copy(acc = account)

      accountNames += name -> address
      accounts += address -> xAccount
      programs += codeHash -> Program(result.returnData)
      storages ++= result.storages
    } else {
      accounts -= address
    }

    (address, result)
  }

  def deleteAccount(name: String): Unit = {
    accountNames.get(name).foreach(accounts -= _)
    accountNames -= name
  }

  def deleteAccount(address: Address): Unit = {
    accounts.get(address).foreach(accountNames -= _.name)
    accounts -= address
  }

  def runTransaction(xAccount: XAccount, callData: ByteString, gas: BigInt, value: BigInt): ProgramResult = {
    val tx = MockVmInput.transaction(ContractCreator.address, callData, value, gas, receivingAddress = xAccount.address)
    val bh = MockVmInput.blockHeader

    val context = ProgramContext(tx, bh, this)
    val result = VM.run(context)

    if (result.error.isEmpty) {
      applyProgramResult(result)
    }

    result
  }

  def applyProgramResult(result: ProgramResult): Unit = {
    storages ++= result.storages
    result.addressesToDelete.foreach(deleteAccount)
    result.internalTransfers.foreach(makeTransfer)
  }

  def makeTransfer(t: Transfer): Unit = {
    val debited = accounts(t.from)
    val credited = accounts.getOrElse(t.to, XAccount(Account.Empty, newName(), t.to, Nil))

    accounts += t.from -> debited.updateBalance(-t.value)
    accounts += t.to -> credited.updateBalance(t.value)
    accountNames += credited.name -> credited.address
  }

  def newName(): String = {
    val name = f"acc$unnamedPrefix%03d"
    unnamedPrefix += 1
    name
  }

}
