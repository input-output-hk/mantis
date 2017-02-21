package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain.{ Account, Address }
import org.scalacheck.{Arbitrary, Gen}

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

  def getDataWordGen(min: DataWord = DataWord(0), max: DataWord = DataWord.MaxValue): Gen[DataWord] =
    getBigIntGen(min.toBigInt, max.toBigInt).map(DataWord(_))

  def getStackGen(minElems: Int = 0, maxElems: Int = testStackMaxSize, dataWordGen: Gen[DataWord] = getDataWordGen(),
    maxSize: Int = testStackMaxSize): Gen[Stack] =
    for {
      size <- Gen.choose(minElems, maxElems)
      list <- Gen.listOfN(size, dataWordGen)
      stack = Stack.empty(maxSize)
    } yield stack.push(list)

  def getStackGen(elems: Int, dataWordGen: Gen[DataWord]): Gen[Stack] =
    getStackGen(minElems = elems, maxElems = elems, dataWordGen)

  def getStackGen(elems: Int): Gen[Stack] =
    getStackGen(minElems = elems, maxElems = elems, getDataWordGen())

  def getStackGen(elems: Int, maxWord: DataWord): Gen[Stack] =
    getStackGen(minElems = elems, maxElems = elems, dataWordGen = getDataWordGen(max = maxWord), maxSize = testStackMaxSize)

  def getStackGen(maxWord: DataWord): Gen[Stack] =
    getStackGen(dataWordGen = getDataWordGen(max = maxWord), maxSize = testStackMaxSize)

  def getMemoryGen(maxSize: Int = 0): Gen[Memory] =
    getByteStringGen(0, maxSize).map(Memory.empty.store(DataWord(0), _))

  def getStorageGen(maxSize: Int = 0, dataWordGen: Gen[DataWord] = getDataWordGen()): Gen[Storage] =
    getListGen(0, maxSize, dataWordGen).map(Storage.fromSeq)

  def getAccountGen(
    nonceGen: Gen[BigInt] = bigIntGen,
    balanceGen: Gen[BigInt] = bigIntGen,
    storageRootGen: Gen[ByteString] = byteStringOfLengthNGen(100),
    codeHashGen: Gen[ByteString] = byteStringOfLengthNGen(20)
  ): Gen[Account] =
    for {
      nonce <- nonceGen
      balance <- balanceGen
      storageRoot <- storageRootGen
      codeHash <- codeHashGen
    } yield Account(nonce, balance, storageRoot, codeHash)

  def addressGen: Gen[Address] = dataWordGen.map(Address(_))

  case class FakeAccountRetriever(accounts: Map[Address, Account] = Map()) extends AccountRetriever {
    override def getAccount(address: Address): Option[Account] = accounts.get(address)
  }

  def getProgramStateGen(
    stackGen: Gen[Stack] = getStackGen(),
    memGen: Gen[Memory] = getMemoryGen(),
    storageGen: Gen[Storage] = getStorageGen(),
    gasGen: Gen[BigInt] = getBigIntGen(min = DataWord.MaxValue, max = DataWord.MaxValue),
    codeGen: Gen[ByteString] = getByteStringGen(0, 0),
    inputDataGen: Gen[ByteString] = getByteStringGen(0, 0),
    valueGen: Gen[BigInt] = getBigIntGen(),
    ownerAccountGen: Gen[Account] = getAccountGen(),
    execAddrGen: Gen[Address] = addressGen,
    senderAddrGen: Gen[Address] = addressGen,
    ownerAddrGen: Gen[Address] = addressGen
  ): Gen[ProgramState] =
    for {
      stack <- stackGen
      memory <- memGen
      storage <- storageGen
      gas <- gasGen
      code <- codeGen
      inputData <- inputDataGen
      value <- valueGen
      ownerAccount <- ownerAccountGen
      ownerAddr <- ownerAddrGen
      senderAddr <- senderAddrGen
      execAddr <- execAddrGen
      env = ExecEnv(ownerAddr, senderAddr, 0, inputData, execAddr, value, Program(code), null, 0)
      context = ProgramContext(env, startGas = gas, storage, ownerAccount, FakeAccountRetriever())
    } yield ProgramState(context).withStack(stack).withMemory(memory)

}
