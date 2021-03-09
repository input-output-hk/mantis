package io.iohk.ethereum

import akka.util.ByteString
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait ByteGenerators {
  def randomSizeByteArrayGen(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    Gen.choose(minSize, maxSize).flatMap(byteArrayOfNItemsGen)

  def byteArrayOfNItemsGen(n: Int): Gen[Array[Byte]] = Gen.listOfN(n, Arbitrary.arbitrary[Byte]).map(_.toArray)

  def randomSizeByteStringGen(minSize: Int, maxSize: Int): Gen[ByteString] =
    Gen.choose(minSize, maxSize).flatMap(byteStringOfLengthNGen)

  def byteStringOfLengthNGen(n: Int): Gen[ByteString] = byteArrayOfNItemsGen(n).map(ByteString(_))

  def seqByteStringOfNItemsOfLengthMGen(seqSize: Int, byteStringLength: Int): Gen[Seq[ByteString]] =
    Gen.listOfN(seqSize, byteStringOfLengthNGen(byteStringLength))

  def seqByteStringOfNItemsGen(n: Int): Gen[Seq[ByteString]] = Gen.listOf(byteStringOfLengthNGen(n))

  def hexPrefixDecodeParametersGen(): Gen[(Array[Byte], Boolean)] =
    for {
      aByteList <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
      t <- Arbitrary.arbitrary[Boolean]
    } yield (aByteList.toArray, t)

  def keyValueByteStringGen(size: Int): Gen[List[(ByteString, Array[Byte])]] =
    for {
      byteStringList <- Gen.nonEmptyListOf(byteStringOfLengthNGen(size))
      arrayList <- Gen.nonEmptyListOf(byteArrayOfNItemsGen(size))
    } yield byteStringList.zip(arrayList)
}
object ByteGenerators extends ByteGenerators {}
