package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import org.scalacheck.{Arbitrary, Gen}

// scalastyle:off magic.number
object Generators extends ObjectGenerators {
  val testStackMaxSize = 32

  def getListGen[T](minSize: Int, maxSize: Int, genT: Gen[T]): Gen[List[T]] =
    Gen.choose(minSize, maxSize).flatMap(size => Gen.listOfN(size, genT))

  def getByteStringGen(maxSize: Int, minSize: Int = 0): Gen[ByteString] =
    getListGen(minSize, maxSize, Arbitrary.arbitrary[Byte]).map(l => ByteString(l.toArray))

  def getBigIntGen(min: BigInt = -BigInt(2).pow(255), max: BigInt = BigInt(2).pow(255) - 1): Gen[BigInt] = {
    val nBits = math.max(min.bitLength, max.bitCount)
    val nBytes = nBits / 8 + (if (nBits % 8 > 0) 1 else 0) - 1
    for {
      byte <- Arbitrary.arbitrary[Byte]
      head = if (min >= 0) byte & 0x7f else if (max < 0) byte | 0x80 else byte
      tail <- getByteStringGen(nBytes, nBytes)
      bigInt = BigInt(head.toByte +: tail.toArray)
      if bigInt >= min && bigInt <= max
    } yield bigInt
  }

  def getDataWordGen(min: DataWord = DataWord(0), max: DataWord = DataWord.MaxWord): Gen[DataWord] =
    getBigIntGen(min.toBigInt, max.toBigInt).map(DataWord(_))

  def getStackGen(dataWordGen: Gen[DataWord] = getDataWordGen(), maxSize: Int = testStackMaxSize): Gen[Stack] =
    for {
      size <- Gen.choose(0, maxSize)
      list <- Gen.listOfN(size, dataWordGen)
      stack = Stack.empty(maxSize)
      _ = println(s"stack: $size, $maxSize")
    } yield stack.push(list).right.get

  def getStackGen(maxWord: DataWord): Gen[Stack] =
    getStackGen(getDataWordGen(max = maxWord))

  def getMemoryGen(maxSize: Int = 0): Gen[Memory] =
    getByteStringGen(maxSize).map(Memory().store(DataWord(0), _))

  def getProgramStateGen(
    stackGen: Gen[Stack] = getStackGen(),
    memGen: Gen[Memory] = getMemoryGen(),
    codeGen: Gen[ByteString] = getByteStringGen(0),
    callDataGen: Gen[ByteString] = getByteStringGen(0)
  ) : Gen[ProgramState] =
    for {
      stack <- stackGen
      memory <- memGen
      code <- codeGen
      callData <- callDataGen
      invoke = ProgramInvoke(new Program(code), callData, ByteString.empty, Storage())
    } yield ProgramState(invoke).withStack(stack).withMemory(memory)

}
