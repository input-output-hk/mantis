package io.iohk.ethereum

import java.math.BigInteger

import io.iohk.ethereum.vm.DataWord
import org.scalacheck.{Arbitrary, Gen}

trait ObjectGenerators {

  def intGen: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)

  def bigIntGen: Gen[BigInt] = byteArrayOfNItemsGen(32).map(b => new BigInteger(1, b))

  def dataWordGen: Gen[DataWord] = bigIntGen.map(DataWord(_))

  def randomSizeByteArrayGen(minSize: Int, maxSize: Int): Gen[Array[Byte]] = Gen.choose(minSize, maxSize).flatMap(byteArrayOfNItemsGen(_))

  def byteArrayOfNItemsGen(n: Int): Gen[Array[Byte]] = Gen.listOfN(n, Arbitrary.arbitrary[Byte]).map(_.toArray)

  def seqByteArrayOfNItemsGen(n: Int): Gen[Seq[Array[Byte]]] = Gen.listOf(byteArrayOfNItemsGen(n))

  def hexPrefixDecodeParametersGen(): Gen[(Array[Byte], Boolean)] = {
    for {
      aByteList <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
      t <- Arbitrary.arbitrary[Boolean]
    } yield (aByteList.toArray, t)
  }

  def keyValueListGen(): Gen[List[(Int, Int)]] = {
    for {
      aKeyList <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Int]).map(_.distinct)
    } yield aKeyList.zip(aKeyList)
  }

}
