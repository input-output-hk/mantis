package io.iohk.ethereum.vm

import akka.util.ByteString

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import io.iohk.ethereum.Fixtures.{Blocks => BlockFixtures}
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.vm.MockWorldState._

import Fixtures.blockchainConfig

// scalastyle:off magic.number
object Generators extends ObjectGenerators {
  val testStackMaxSize = 32

  def getListGen[T](minSize: Int, maxSize: Int, genT: Gen[T]): Gen[List[T]] =
    Gen.choose(minSize, maxSize).flatMap(size => Gen.listOfN(size, genT))

  def getByteStringGen(minSize: Int, maxSize: Int, byteGen: Gen[Byte] = Arbitrary.arbitrary[Byte]): Gen[ByteString] =
    getListGen(minSize, maxSize, byteGen).map(l => ByteString(l.toArray))

  def getBigIntGen(min: BigInt = 0, max: BigInt = BigInt(2).pow(256) - 1): Gen[BigInt] = {
    val mod = max - min
    val nBytes = mod.bitLength / 8 + 1
    for {
      byte <- Arbitrary.arbitrary[Byte]
      bytes <- getByteStringGen(nBytes, nBytes)
      bigInt = (if (mod > 0) BigInt(bytes.toArray).abs % mod else BigInt(0)) + min
    } yield bigInt
  }

  def getUInt256Gen(min: UInt256 = UInt256(0), max: UInt256 = UInt256.MaxValue): Gen[UInt256] =
    getBigIntGen(min.toBigInt, max.toBigInt).map(UInt256(_))

  def getStackGen(
      minElems: Int = 0,
      maxElems: Int = testStackMaxSize,
      valueGen: Gen[UInt256] = getUInt256Gen(),
      maxSize: Int = testStackMaxSize
  ): Gen[Stack] =
    for {
      size <- Gen.choose(minElems, maxElems)
      list <- Gen.listOfN(size, valueGen)
      stack = Stack.empty(maxSize)
    } yield stack.push(list)

  def getStackGen(elems: Int, uint256Gen: Gen[UInt256]): Gen[Stack] =
    getStackGen(minElems = elems, maxElems = elems, uint256Gen)

  def getStackGen(elems: Int): Gen[Stack] =
    getStackGen(minElems = elems, maxElems = elems, getUInt256Gen())

  def getStackGen(elems: Int, maxUInt: UInt256): Gen[Stack] =
    getStackGen(minElems = elems, maxElems = elems, valueGen = getUInt256Gen(max = maxUInt), maxSize = testStackMaxSize)

  def getStackGen(maxWord: UInt256): Gen[Stack] =
    getStackGen(valueGen = getUInt256Gen(max = maxWord), maxSize = testStackMaxSize)

  def getMemoryGen(maxSize: Int = 0): Gen[Memory] =
    getByteStringGen(0, maxSize).map(Memory.empty.store(0, _))

  def getStorageGen(maxSize: Int = 0, uint256Gen: Gen[UInt256] = getUInt256Gen()): Gen[MockStorage] =
    getListGen(0, maxSize, uint256Gen).map(MockStorage.fromSeq)

  val ownerAddr: Address = Address(0x123456)
  val callerAddr: Address = Address(0xabcdef)

  val exampleBlockHeader = BlockFixtures.ValidBlock.header

  // scalastyle:off
  def getProgramStateGen(
      stackGen: Gen[Stack] = getStackGen(),
      memGen: Gen[Memory] = getMemoryGen(),
      storageGen: Gen[MockStorage] = getStorageGen(),
      gasGen: Gen[BigInt] = getBigIntGen(min = UInt256.MaxValue.toBigInt, max = UInt256.MaxValue.toBigInt),
      codeGen: Gen[ByteString] = getByteStringGen(0, 0),
      inputDataGen: Gen[ByteString] = getByteStringGen(0, 0),
      valueGen: Gen[UInt256] = getUInt256Gen(),
      blockNumberGen: Gen[UInt256] = getUInt256Gen(0, 300),
      evmConfig: EvmConfig = EvmConfig.PhoenixConfigBuilder(blockchainConfig),
      returnDataGen: Gen[ByteString] = getByteStringGen(0, 0),
      isTopHeader: Boolean = false
  ): Gen[PS] =
    for {
      stack <- stackGen
      memory <- memGen
      storage <- storageGen
      gas <- gasGen
      code <- codeGen
      inputData <- inputDataGen
      value <- valueGen
      blockNumber <- blockNumberGen
      blockPlacement <- getUInt256Gen(0, blockNumber)
      returnData <- returnDataGen

      blockHeader = exampleBlockHeader.copy(number = if (isTopHeader) blockNumber else blockNumber - blockPlacement)

      world = MockWorldState(numberOfHashes = blockNumber - 1)
        .saveCode(ownerAddr, code)
        .saveStorage(ownerAddr, storage)
        .saveAccount(ownerAddr, Account.empty().increaseBalance(value))

      context: PC = ProgramContext(
        callerAddr = callerAddr,
        originAddr = callerAddr,
        recipientAddr = Some(ownerAddr),
        gasPrice = 0,
        startGas = gas,
        inputData = inputData,
        value = value,
        endowment = value,
        blockHeader = blockHeader,
        doTransfer = true,
        callDepth = 0,
        world = world,
        initialAddressesToDelete = Set(),
        evmConfig = evmConfig,
        originalWorld = world
      )

      env = ExecEnv(context, code, ownerAddr)

      vm = new TestVM

    } yield ProgramState(vm, context, env).withStack(stack).withMemory(memory).withReturnData(returnData)

}
