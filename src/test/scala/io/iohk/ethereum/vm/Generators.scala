package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain.{Account, Address, BlockHeader}
import io.iohk.ethereum.vm.MockWorldState._
import org.scalacheck.{Arbitrary, Gen}
import org.spongycastle.util.encoders.Hex

// scalastyle:off magic.number
object Generators extends ObjectGenerators {
  val testStackMaxSize = 32

  def getListGen[T](minSize: Int, maxSize: Int, genT: Gen[T]): Gen[List[T]] =
    Gen.choose(minSize, maxSize).flatMap(size => Gen.listOfN(size, genT))

  def getByteStringGen(minSize: Int, maxSize: Int): Gen[ByteString] =
    getListGen(minSize, maxSize, Arbitrary.arbitrary[Byte]).map(l => ByteString(l.toArray))

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

  def getStackGen(minElems: Int = 0, maxElems: Int = testStackMaxSize, uint256Gen: Gen[UInt256] = getUInt256Gen(),
    maxSize: Int = testStackMaxSize): Gen[Stack] =
    for {
      size <- Gen.choose(minElems, maxElems)
      list <- Gen.listOfN(size, uint256Gen)
      stack = Stack.empty(maxSize)
    } yield stack.push(list)

  def getStackGen(elems: Int, uint256Gen: Gen[UInt256]): Gen[Stack] =
    getStackGen(minElems = elems, maxElems = elems, uint256Gen)

  def getStackGen(elems: Int): Gen[Stack] =
    getStackGen(minElems = elems, maxElems = elems, getUInt256Gen())

  def getStackGen(elems: Int, maxUInt: UInt256): Gen[Stack] =
    getStackGen(minElems = elems, maxElems = elems, uint256Gen = getUInt256Gen(max = maxUInt), maxSize = testStackMaxSize)

  def getStackGen(maxWord: UInt256): Gen[Stack] =
    getStackGen(uint256Gen = getUInt256Gen(max = maxWord), maxSize = testStackMaxSize)

  def getMemoryGen(maxSize: Int = 0): Gen[Memory] =
    getByteStringGen(0, maxSize).map(Memory.empty.store(UInt256(0), _))

  def getStorageGen(maxSize: Int = 0, uint256Gen: Gen[UInt256] = getUInt256Gen()): Gen[MockStorage] =
    getListGen(0, maxSize, uint256Gen).map(MockStorage.fromSeq)

  val ownerAddr = Address(0x123456)
  val callerAddr = Address(0xabcdef)

  val exampleBlockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("d882d5c210bab4cb7ef0b9f3dc2130cb680959afcd9a8f9bf83ee6f13e2f9da3")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("95f484419881c6e9b6de7fb3f8ad03763bd49a89")),
    stateRoot = ByteString(Hex.decode("634a2b20c9e02afdda7157afe384306c5acc4fb9c09b45dc0203c0fbb2fed0e6")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = BigInt("989772"),
    number = 20,
    gasLimit = 131620495,
    gasUsed = 0,
    unixTimestamp = 1486752441,
    extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
    mixHash = ByteString(Hex.decode("6bc729364c9b682cfa923ba9480367ebdfa2a9bca2a652fe975e8d5958f696dd")),
    nonce = ByteString(Hex.decode("797a8f3a494f937b")))

  def getProgramStateGen(
    stackGen: Gen[Stack] = getStackGen(),
    memGen: Gen[Memory] = getMemoryGen(),
    storageGen: Gen[MockStorage] = getStorageGen(),
    gasGen: Gen[UInt256] = getUInt256Gen(min = UInt256.MaxValue, max = UInt256.MaxValue),
    codeGen: Gen[ByteString] = getByteStringGen(0, 0),
    inputDataGen: Gen[ByteString] = getByteStringGen(0, 0),
    valueGen: Gen[UInt256] = getUInt256Gen(),
    blockNumberGen: Gen[UInt256] = getUInt256Gen(0, 300)
  ): Gen[PS] =
    for {
      stack <- stackGen
      memory <- memGen
      storage <- storageGen
      gas <- gasGen
      program <- codeGen.map(Program.apply)
      inputData <- inputDataGen
      value <- valueGen
      blockNumber <- blockNumberGen
      blockPlacement <- getUInt256Gen(0, blockNumber)

      blockHeader = exampleBlockHeader.copy(number = blockNumber - blockPlacement)

      env = ExecEnv(ownerAddr, callerAddr, callerAddr, 0, inputData,
        value, program, blockHeader, 0)

      world = MockWorldState(numberOfHashes = blockNumber - 1)
        .saveCode(ownerAddr, program.code)
        .saveStorage(ownerAddr, storage)
        .saveAccount(ownerAddr, Account.Empty)

      context: PC = ProgramContext(env, startGas = gas, world, EvmConfig.PostEIP160Config)
    } yield ProgramState(context).withStack(stack).withMemory(memory)

}
