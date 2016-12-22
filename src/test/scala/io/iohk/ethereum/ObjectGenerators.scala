package io.iohk.ethereum

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen, _}

import scala.collection.mutable.ListBuffer

trait ObjectGenerators {
  def hexPrefixDecodeParametersGen(): Gen[(Array[Byte], Boolean)] = for {
    aByteList <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    t <- Arbitrary.arbitrary[Boolean]
  } yield (aByteList.toArray, t)

  def keyValueListGen(): Gen[List[(Int, Int)]] = for {
    aKeyList <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Int]).map(_.distinct)
  } yield aKeyList.zip(aKeyList)
}
