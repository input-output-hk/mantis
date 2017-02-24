package io.iohk.ethereum.vmrunner

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.vm._
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.RLPImplicits._

object State {

  val creatorAddress = Address(DataWord(0xabcdef))

  private var currentWorld = {
    val creatorAcc = Account.Empty.updateBalance(BigInt(10).pow(9))
    val creatorXAcc = XAccount(creatorAcc, "Creator", creatorAddress, Nil)
    WorldState().saveXAccount(creatorAddress, creatorXAcc)
  }

  def world: WorldState = currentWorld


  def createAccount(name: String, balance: BigInt, gas: BigInt, code: ByteString, abis: Seq[ABI]): (Address, ProgramResult) = {
    val (newAddress, _) = world.newAddress(creatorAddress)
    val tx = MockVmInput.transaction(creatorAddress, code, balance, gas)
    val bh = MockVmInput.blockHeader

    val context = ProgramContext(tx, bh, world)
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
      val codeHash = ByteString(kec256(result.returnData.toArray))
      val account = result.world.getGuaranteedAccount(newAddress).copy(codeHash = codeHash)
      val xAccount = XAccount(account, name, newAddress, abis)

      val world1 = result.world.asInstanceOf[WorldState]
        .saveXAccount(newAddress, xAccount)
        .saveCode(codeHash, result.returnData)

      val world2 = result.addressesToDelete.foldLeft(world1)(_ deleteAccount _)

      currentWorld = world2
    }

    (newAddress, result)
  }

  def runTransaction(xAccount: XAccount, callData: ByteString, gas: BigInt, value: BigInt): ProgramResult = {
    val tx = MockVmInput.transaction(creatorAddress, callData, value, gas, receivingAddress = xAccount.address)
    val bh = MockVmInput.blockHeader

    val context = ProgramContext(tx, bh, world)
    val result = VM.run(context)

    if (result.error.isEmpty) {
      val world = result.world.asInstanceOf[WorldState]
      val world1 = result.addressesToDelete.foldLeft(world)(_ deleteAccount _)
      currentWorld = world1
    }

    result
  }
}
