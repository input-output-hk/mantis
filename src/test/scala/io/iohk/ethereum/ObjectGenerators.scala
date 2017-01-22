package io.iohk.ethereum

import akka.util.ByteString
import java.math.BigInteger
import org.scalacheck.{Arbitrary, Gen}

trait ObjectGenerators {

  def intGen: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)

  def bigIntGen: Gen[BigInt] = byteArrayOfNItemsGen(32).map(b => new BigInteger(1, b))

  // def byteStringGen(minSize: Int = 0, maxSize: Int = 32): Gen[ByteString] = {
  //   for {
  //     size <- Gen.choose(minSize, maxSize)
  //     byteArray <- byteArrayOfNItemsGen(size)
  //   } yield {
  //     ByteString(byteArray)
  //   }
  // }

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
