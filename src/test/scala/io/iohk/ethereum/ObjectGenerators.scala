package io.iohk.ethereum

import java.math.BigInteger
import org.scalacheck.{Arbitrary, Gen, _}

trait ObjectGenerators {

  lazy val intGen = Gen.choose(Int.MinValue, Int.MaxValue)

  lazy val bigIntGen = byteArrayOfNItemsGen(32).map(b=>new BigInteger(1, b))

  lazy val anyArrayGen = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte]).map(_.toArray)

  def byteArrayOfNItemsGen(n: Int) = Gen.listOfN(n, Arbitrary.arbitrary[Byte]).map(_.toArray)

  def hexPrefixDecodeParametersGen(): Gen[(Array[Byte], Boolean)] = for {
    aByteList <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    t <- Arbitrary.arbitrary[Boolean]
  } yield (aByteList.toArray, t)

  def keyValueListGen(): Gen[List[(Int, Int)]] = for {
    aKeyList <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Int]).map(_.distinct)
  } yield aKeyList.zip(aKeyList)

}
