package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import org.scalacheck.{Arbitrary, Gen}

// scalastyle:off magic.number
object Generators extends ObjectGenerators {
  val testStackMaxSize = 32

  def getListGen[T](minSize: Int, maxSize: Int, genT: Gen[T]): Gen[List[T]] =
    Gen.choose(minSize, maxSize).flatMap(size => Gen.listOfN(size, genT))

  def getByteStringGen(minSize: Int, maxSize: Int): Gen[ByteString] =
    getListGen(minSize, maxSize, Arbitrary.arbitrary[Byte]).map(l => ByteString(l.toArray))

  def getBigIntGen(min: BigInt = -BigInt(2).pow(255), max: BigInt = BigInt(2).pow(255) - 1): Gen[BigInt] = {
    val mod = max - min
    val nBytes = mod.bitLength / 8 + 1
    for {
      byte <- Arbitrary.arbitrary[Byte]
      bytes <- getByteStringGen(nBytes, nBytes)
      bigInt = BigInt(bytes.toArray).abs % mod + min
    } yield bigInt
  }

  def getDataWordGen(min: DataWord = DataWord(0), max: DataWord = DataWord.MaxWord): Gen[DataWord] =
    getBigIntGen(min.toBigInt, max.toBigInt).map(DataWord(_))

  def getStackGen(dataWordGen: Gen[DataWord] = getDataWordGen(), maxSize: Int = testStackMaxSize): Gen[Stack] =
    for {
      size <- Gen.choose(0, maxSize)
      list <- Gen.listOfN(size, dataWordGen)
      stack = Stack.empty(maxSize)
    } yield stack.push(list)

  def getStackGen(maxWord: DataWord): Gen[Stack] =
    getStackGen(getDataWordGen(max = maxWord))

  def getMemoryGen(maxSize: Int = 0): Gen[Memory] =
    getByteStringGen(0, maxSize).map(Memory.empty.store(DataWord(0), _))

  def getStorageGen(maxSize: Int = 0): Gen[Storage] =
    getListGen(0, maxSize, getDataWordGen()).map(Storage.fromSeq)

  def getProgramStateGen(
    stackGen: Gen[Stack] = getStackGen(),
    memGen: Gen[Memory] = getMemoryGen(),
    storageGen: Gen[Storage] = getStorageGen(),
    codeGen: Gen[ByteString] = getByteStringGen(0, 0),
    callDataGen: Gen[ByteString] = getByteStringGen(0, 0),
    callValueGen: Gen[ByteString] = getByteStringGen(0, 32)
  ) : Gen[ProgramState] =
    for {
      stack <- stackGen
      memory <- memGen
      storage <- storageGen
      code <- codeGen
      callData <- callDataGen
      callValue <- callValueGen
      context = ProgramContext(Program(code), DataWord.MaxWord.toBigInt, callData, callValue, storage)
    } yield ProgramState(context).withStack(stack).withMemory(memory)

}
