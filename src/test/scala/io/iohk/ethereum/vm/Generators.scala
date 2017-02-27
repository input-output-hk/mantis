package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain.Address
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

  def getUInt256Gen(min: BigInt = BigInt(0), max: BigInt = Int256Like.Modulus - 1): Gen[UInt256] =
    getBigIntGen(min, max).map(UInt256(_))

  def getInt256Gen(min: BigInt = BigInt(0), max: BigInt = Int256Like.Modulus - 1): Gen[Int256] =
    getBigIntGen(min, max).map(Int256(_))

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

  def getProgramStateGen(
    stackGen: Gen[Stack] = getStackGen(),
    memGen: Gen[Memory] = getMemoryGen(),
    storageGen: Gen[Storage] = getStorageGen(),
    gasGen: Gen[BigInt] = getBigIntGen(min = DataWord.MaxValue, max = DataWord.MaxValue),
    codeGen: Gen[ByteString] = getByteStringGen(0, 0),
    inputDataGen: Gen[ByteString] = getByteStringGen(0, 0),
    valueGen: Gen[BigInt] = getBigIntGen()
  ): Gen[ProgramState] =
    for {
      stack <- stackGen
      memory <- memGen
      storage <- storageGen
      gas <- gasGen
      code <- codeGen
      inputData <- inputDataGen
      value <- valueGen
      env = ExecEnv(Address.empty, Address.empty, 0, inputData,
        Address.empty, value, Program(code), null, 0)
      context = ProgramContext(env, startGas = gas, storage)
    } yield ProgramState(context).withStack(stack).withMemory(memory)

}
