package io.iohk.ethereum.vm

import akka.util.ByteString

import io.iohk.ethereum.Fixtures.{Blocks => BlockFixtures}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.utils.ByteStringUtils._
import io.iohk.ethereum.vm.MockWorldState._

class CallOpFixture(val config: EvmConfig, val startState: MockWorldState) {
  import config.feeSchedule._

  val ownerAddr: Address = Address(0xcafebabe)
  val extAddr: Address = Address(0xfacefeed)
  val callerAddr: Address = Address(0xdeadbeef)

  val ownerOffset: UInt256 = UInt256(0)
  val callerOffset: UInt256 = UInt256(1)
  val valueOffset: UInt256 = UInt256(2)

  val extCode: Assembly = Assembly(
    //store owner address
    ADDRESS,
    PUSH1,
    ownerOffset.toInt,
    SSTORE,
    //store caller address
    CALLER,
    PUSH1,
    callerOffset.toInt,
    SSTORE,
    //store call value
    CALLVALUE,
    PUSH1,
    valueOffset.toInt,
    SSTORE,
    // return first half of unmodified input data
    PUSH1,
    2,
    CALLDATASIZE,
    DIV,
    PUSH1,
    0,
    DUP2,
    DUP2,
    DUP1,
    CALLDATACOPY,
    RETURN
  )

  val selfDestructCode: Assembly = Assembly(
    PUSH20,
    callerAddr.bytes,
    SELFDESTRUCT
  )

  val selfDestructTransferringToSelfCode: Assembly = Assembly(
    PUSH20,
    extAddr.bytes,
    SELFDESTRUCT
  )

  val sstoreWithClearCode: Assembly = Assembly(
    //Save a value to the storage
    PUSH1,
    10,
    PUSH1,
    0,
    SSTORE,
    //Clear the store
    PUSH1,
    0,
    PUSH1,
    0,
    SSTORE
  )

  val valueToReturn = 23
  val returnSingleByteProgram: Assembly = Assembly(
    PUSH1,
    valueToReturn,
    PUSH1,
    0,
    MSTORE,
    PUSH1,
    1,
    PUSH1,
    31,
    RETURN
  )

  val revertProgram: Assembly = Assembly(
    PUSH1,
    valueToReturn,
    PUSH1,
    0,
    MSTORE,
    PUSH1,
    1,
    PUSH1,
    31,
    REVERT
  )

  val inputData: ByteString = Generators.getUInt256Gen().sample.get.bytes
  val expectedMemCost: BigInt = config.calcMemCost(inputData.size, inputData.size, inputData.size / 2)

  val initialBalance: UInt256 = UInt256(1000)

  val requiredGas: BigInt = {
    val storageCost = 3 * G_sset
    val memCost = config.calcMemCost(0, 0, 32)
    val copyCost = G_copy * wordsForBytes(32)

    extCode.linearConstGas(config) + storageCost + memCost + copyCost
  }

  val gasMargin = 13

  val initialOwnerAccount: Account = Account(balance = initialBalance)

  val extProgram = extCode.program
  val invalidProgram: Program = Program(concatByteStrings(extProgram.code.init, INVALID.code))
  val selfDestructProgram = selfDestructCode.program
  val sstoreWithClearProgram = sstoreWithClearCode.program
  val accountWithCode: ByteString => Account = code => Account.empty().withCode(kec256(code))

  val worldWithoutExtAccount: MockWorldState = startState.saveAccount(ownerAddr, initialOwnerAccount)

  val worldWithExtAccount: MockWorldState = worldWithoutExtAccount
    .saveAccount(extAddr, accountWithCode(extProgram.code))
    .saveCode(extAddr, extProgram.code)

  val worldWithExtEmptyAccount: MockWorldState = worldWithoutExtAccount.saveAccount(extAddr, Account.empty())

  val worldWithInvalidProgram: MockWorldState = worldWithoutExtAccount
    .saveAccount(extAddr, accountWithCode(invalidProgram.code))
    .saveCode(extAddr, invalidProgram.code)

  val worldWithSelfDestructProgram: MockWorldState = worldWithoutExtAccount
    .saveAccount(extAddr, accountWithCode(selfDestructProgram.code))
    .saveCode(extAddr, selfDestructCode.code)

  val worldWithRevertProgram: MockWorldState = worldWithoutExtAccount
    .saveAccount(extAddr, accountWithCode(revertProgram.code))
    .saveCode(extAddr, revertProgram.code)

  val worldWithSelfDestructSelfProgram: MockWorldState = worldWithoutExtAccount
    .saveAccount(extAddr, Account.empty())
    .saveCode(extAddr, selfDestructTransferringToSelfCode.code)

  val worldWithSstoreWithClearProgram: MockWorldState = worldWithoutExtAccount
    .saveAccount(extAddr, accountWithCode(sstoreWithClearProgram.code))
    .saveCode(extAddr, sstoreWithClearCode.code)

  val worldWithReturnSingleByteCode: MockWorldState = worldWithoutExtAccount
    .saveAccount(extAddr, accountWithCode(returnSingleByteProgram.code))
    .saveCode(extAddr, returnSingleByteProgram.code)

  val fakeHeader: BlockHeader = BlockFixtures.ValidBlock.header.copy(number = 0, unixTimestamp = 0)

  lazy val context: PC = ProgramContext(
    callerAddr = callerAddr,
    originAddr = callerAddr,
    recipientAddr = Some(ownerAddr),
    gasPrice = 1,
    startGas = 2 * requiredGas,
    inputData = ByteString.empty,
    value = 123,
    endowment = 123,
    doTransfer = true,
    blockHeader = fakeHeader,
    callDepth = 0,
    world = worldWithExtAccount,
    initialAddressesToDelete = Set(),
    evmConfig = config,
    originalWorld = worldWithExtAccount,
    warmAddresses = Set.empty,
    warmStorage = Set.empty
  )

  case class ExecuteCall(
      op: CallOp,
      context: PC = context,
      inputData: ByteString = inputData,
      gas: BigInt = requiredGas + gasMargin,
      to: Address = extAddr,
      value: UInt256 = initialBalance / 2,
      inOffset: UInt256 = UInt256.Zero,
      inSize: UInt256 = inputData.size,
      outOffset: UInt256 = inputData.size,
      outSize: UInt256 = inputData.size / 2,
      toAlreadyAccessed: Boolean = false
  ) extends CallResult {

    val vm = new TestVM

    val env: ExecEnv = ExecEnv(context, ByteString.empty, ownerAddr)

    private val params = Seq(UInt256(gas), to.toUInt256, value, inOffset, inSize, outOffset, outSize).reverse

    private val paramsForDelegate = params.take(4) ++ params.drop(5)

    private val stack = Stack.empty().push(if (op == DELEGATECALL) paramsForDelegate else params)
    private val mem = Memory.empty.store(UInt256.Zero, inputData)

    val baseStateIn: PS = ProgramState(vm, context, env).withStack(stack).withMemory(mem)
    val stateIn: PS = if (toAlreadyAccessed) baseStateIn.addAccessedAddress(to) else baseStateIn
    val stateOut: PS = op.execute(stateIn)
    val world: MockWorldState = stateOut.world

    val ownBalance: UInt256 = world.getBalance(env.ownerAddr)
    val extBalance: UInt256 = world.getBalance(to)

    val ownStorage: MockStorage = world.getStorage(env.ownerAddr)
    val extStorage: MockStorage = world.getStorage(to)
  }
}

protected[vm] trait CallResult {
  def inputData: ByteString
  def stateOut: PS
  def world: MockWorldState
  def ownBalance: UInt256
  def extBalance: UInt256
  def ownStorage: MockStorage
  def extStorage: MockStorage
  def outOffset: UInt256
  def outSize: UInt256
  def value: UInt256
}
