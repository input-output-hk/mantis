package io.iohk.ethereum.rlp

import akka.util.ByteString
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.rlp.RLPEncoding._
import io.iohk.ethereum.rlp.RLPEncoding.Implicits._

import scala.util.Try

class RLPSuite extends FunSuite
  with PropertyChecks
  with GeneratorDrivenPropertyChecks {

  test("nextElementIndex of empty data"){
    val maybeIndex = Try{RLPEncoding.nextElementIndex(Array.emptyByteArray, 0)}
    assert(maybeIndex.isFailure)
  }

  test("Decoding of empty data"){
    val maybeDecoded = Try{RLPEncoding.decode[Array[Byte]](Array.emptyByteArray)}
    assert(maybeDecoded.isFailure)
  }

  test("Decoding failure: Passing RLPValue when RLPList is expected"){
    val data = RLPEncoding.encode(0.toLong)
    val maybeSeqObtained = Try{RLPEncoding.decode[Seq[Long]](data)(seqEncDec())}
    assert(maybeSeqObtained.isFailure)
  }

  test("Decoding failure: Passing RLPList when RLPValue is expected"){
    val data = RLP.encode(RLPList("cat", "dog"))
    val maybeByteObtained = Try{RLPEncoding.decode[Byte](data)}
    val maybeShortObtained = Try{RLPEncoding.decode[Short](data)}
    val maybeIntObtained = Try{RLPEncoding.decode[Int](data)}
    val maybeLongObtained = Try{RLPEncoding.decode[Long](data)}
    val maybeBigIntObtained = Try{RLPEncoding.decode[BigInt](data)}
    val maybeStringObtained = Try{RLPEncoding.decode[String](data)}
    val maybeByteArrayObtained = Try{RLPEncoding.decode[Array[Byte]](data)}
    assert(maybeByteObtained.isFailure)
    assert(maybeShortObtained.isFailure)
    assert(maybeIntObtained.isFailure)
    assert(maybeLongObtained.isFailure)
    assert(maybeStringObtained.isFailure)
    assert(maybeByteArrayObtained.isFailure)
    assert(maybeBigIntObtained.isFailure)
  }

  test("Decoding failure: Passing an RLPValue larger than expected"){
    val num: BigInt = 16 * BigInt(Long.MaxValue)
    val data = RLPEncoding.encode(num)
    val maybeByteObtained = Try{RLPEncoding.decode[Byte](data)}
    val maybeShortObtained = Try{RLPEncoding.decode[Short](data)}
    val maybeIntObtained = Try{RLPEncoding.decode[Int](data)}
    val maybeLongObtained = Try{RLPEncoding.decode[Long](data)}
    assert(maybeByteObtained.isFailure)
    assert(maybeShortObtained.isFailure)
    assert(maybeIntObtained.isFailure)
    assert(maybeLongObtained.isFailure)
  }

  test("Byte Encoding") {
    val expected = Array[Byte](0x80.toByte)
    val data = RLPEncoding.encode(0: Byte)

    assert(expected sameElements data)
    val dataObtained = RLPEncoding.decode[Byte](data)
    val obtained: Byte = dataObtained
    assert((0: Byte) == obtained)

    val expected2 = Array[Byte](0x78.toByte)
    val data2 = RLPEncoding.encode(120: Byte)
    assert(expected2 sameElements data2)
    val dataObtained2 = RLPEncoding.decode[Byte](data2)
    val obtained2: Byte = dataObtained2
    assert((120: Byte) == obtained2)

    val expected3 = Array[Byte](0x7F.toByte)
    val data3 = RLPEncoding.encode(127: Byte)
    assert(expected3 sameElements data3)
    val dataObtained3 = RLPEncoding.decode[Byte](data3)
    val obtained3: Byte = dataObtained3
    assert((127: Byte) == obtained3)

    forAll(Gen.choose[Byte](Byte.MinValue, Byte.MaxValue)) {
      (aByte: Byte) => {
        val data = RLPEncoding.encode(aByte)
        val dataObtained = RLPEncoding.decode[Byte](data)
        val obtained: Byte = dataObtained
        assert(aByte == obtained)
      }
    }
  }

  test("Short Encoding") {
    val expected4 = Array[Byte](0x82.toByte, 0x76.toByte, 0x5F.toByte)
    val data4 = RLPEncoding.encode(30303.toShort)
    assert(expected4 sameElements data4)
    val dataObtained4 = RLPEncoding.decode[Short](data4)
    val obtained4: Short = dataObtained4
    assert((30303: Short) == obtained4)

    val expected5 = Array[Byte](0x82.toByte, 0x4E.toByte, 0xEA.toByte)
    val data5 = RLPEncoding.encode(20202.toShort)
    assert(expected5 sameElements data5)
    val dataObtained5 = RLPEncoding.decode[Short](data5)
    val obtained5: Short = dataObtained5
    assert((20202: Short) == obtained5)

    val expected6 = Array[Byte](0x82.toByte, 0x9D.toByte, 0x0A.toByte)
    val data6 = RLPEncoding.encode(40202.toShort)
    assert(expected6 sameElements data6)
    val dataObtained6 = RLPEncoding.decode[Short](data6)
    val obtained6: Short = dataObtained6
    assert(40202.toShort == obtained6)

    val expected7 = Array[Byte](0x7f.toByte)
    val data7 = RLPEncoding.encode(127.toShort)
    assert(expected7 sameElements data7)
    val dataObtained7 = RLPEncoding.decode[Short](data7)
    val obtained7: Short = dataObtained7
    assert(127.toShort == obtained7)

    val expected8 = Array[Byte](0x80.toByte)
    val data8 = RLPEncoding.encode(0.toShort)
    assert(expected8 sameElements data8)
    val dataObtained8 = RLPEncoding.decode[Short](data8)
    val obtained8: Short = dataObtained8
    assert(0.toShort == obtained8)

    forAll(Gen.choose[Short](Short.MinValue, Short.MaxValue)) {
      (aShort: Short) => {
        val data = RLPEncoding.encode(aShort)
        val dataObtained = RLPEncoding.decode[Short](data)
        val obtained: Short = dataObtained
        assert(aShort == obtained)
      }
    }
  }

  test("String encoding") {
    val expected = Array[Byte](0x80.toByte)
    val data = RLPEncoding.encode("")
    assert(expected sameElements data)
    val dataObtained = RLPEncoding.decode[String](data)
    val obtained: String = dataObtained
    assert("" == obtained)

    val expected2 = Array[Byte](0x90.toByte, 0x45.toByte, 0x74.toByte, 0x68.toByte, 0x65.toByte, 0x72.toByte, 0x65.toByte,
      0x75.toByte, 0x6D.toByte, 0x4A.toByte, 0x20.toByte, 0x43.toByte, 0x6C.toByte, 0x69.toByte, 0x65.toByte, 0x6E.toByte, 0x74.toByte)
    val data2 = RLPEncoding.encode("EthereumJ Client")
    assert(expected2 sameElements data2)
    val dataObtained2 = RLPEncoding.decode[String](data2)
    val obtained2: String = dataObtained2
    assert("EthereumJ Client" == obtained2)

    val expected3 = Array[Byte](
      0xAD.toByte, 0x45.toByte, 0x74.toByte, 0x68.toByte, 0x65.toByte, 0x72.toByte, 0x65.toByte,
      0x75.toByte, 0x6D.toByte, 0x28.toByte, 0x2B.toByte, 0x2B.toByte, 0x29.toByte, 0x2F.toByte,
      0x5A.toByte, 0x65.toByte, 0x72.toByte, 0x6F.toByte, 0x47.toByte, 0x6F.toByte, 0x78.toByte,
      0x2F.toByte, 0x76.toByte, 0x30.toByte, 0x2E.toByte, 0x35.toByte, 0x2E.toByte, 0x30.toByte,
      0x2F.toByte, 0x6E.toByte, 0x63.toByte, 0x75.toByte, 0x72.toByte, 0x73.toByte, 0x65.toByte,
      0x73.toByte, 0x2F.toByte, 0x4C.toByte, 0x69.toByte, 0x6E.toByte, 0x75.toByte, 0x78.toByte,
      0x2F.toByte, 0x67.toByte, 0x2B.toByte, 0x2B.toByte
    )
    val data3 = RLPEncoding.encode("Ethereum(++)/ZeroGox/v0.5.0/ncurses/Linux/g++")
    assert(expected3 sameElements data3)
    val dataObtained3 = RLPEncoding.decode[String](data3)
    val obtained3: String = dataObtained3
    assert("Ethereum(++)/ZeroGox/v0.5.0/ncurses/Linux/g++" == obtained3)

    val expected4 = Array[Byte](
      0xB8.toByte, 0x5A.toByte,
      0x45.toByte, 0x74.toByte, 0x68.toByte, 0x65.toByte, 0x72.toByte, 0x65.toByte,
      0x75.toByte, 0x6D.toByte, 0x28.toByte, 0x2B.toByte, 0x2B.toByte, 0x29.toByte, 0x2F.toByte,
      0x5A.toByte, 0x65.toByte, 0x72.toByte, 0x6F.toByte, 0x47.toByte, 0x6F.toByte, 0x78.toByte,
      0x2F.toByte, 0x76.toByte, 0x30.toByte, 0x2E.toByte, 0x35.toByte, 0x2E.toByte, 0x30.toByte,
      0x2F.toByte, 0x6E.toByte, 0x63.toByte, 0x75.toByte, 0x72.toByte, 0x73.toByte, 0x65.toByte,
      0x73.toByte, 0x2F.toByte, 0x4C.toByte, 0x69.toByte, 0x6E.toByte, 0x75.toByte, 0x78.toByte,
      0x2F.toByte, 0x67.toByte, 0x2B.toByte, 0x2B.toByte,

      0x45.toByte, 0x74.toByte, 0x68.toByte, 0x65.toByte, 0x72.toByte, 0x65.toByte,
      0x75.toByte, 0x6D.toByte, 0x28.toByte, 0x2B.toByte, 0x2B.toByte, 0x29.toByte, 0x2F.toByte,
      0x5A.toByte, 0x65.toByte, 0x72.toByte, 0x6F.toByte, 0x47.toByte, 0x6F.toByte, 0x78.toByte,
      0x2F.toByte, 0x76.toByte, 0x30.toByte, 0x2E.toByte, 0x35.toByte, 0x2E.toByte, 0x30.toByte,
      0x2F.toByte, 0x6E.toByte, 0x63.toByte, 0x75.toByte, 0x72.toByte, 0x73.toByte, 0x65.toByte,
      0x73.toByte, 0x2F.toByte, 0x4C.toByte, 0x69.toByte, 0x6E.toByte, 0x75.toByte, 0x78.toByte,
      0x2F.toByte, 0x67.toByte, 0x2B.toByte, 0x2B.toByte
    )
    val data4 = RLPEncoding.encode("Ethereum(++)/ZeroGox/v0.5.0/ncurses/Linux/g++Ethereum(++)/ZeroGox/v0.5.0/ncurses/Linux/g++")
    assert(expected4 sameElements data4)
    val dataObtained4 = RLPEncoding.decode[String](data4)
    val obtained4: String = dataObtained4
    assert("Ethereum(++)/ZeroGox/v0.5.0/ncurses/Linux/g++Ethereum(++)/ZeroGox/v0.5.0/ncurses/Linux/g++" == obtained4)

    val strGen = (n: Int) => Gen.choose(0, n).flatMap(long => Gen.listOfN(long, Gen.alphaChar).map(_.mkString))

    forAll(strGen(10000)) {
      (aString: String) => {
        val data = RLPEncoding.encode(aString)
        val dataObtained = RLPEncoding.decode[String](data)
        val obtained: String = dataObtained
        assert(aString == obtained)
      }
    }
  }

  test("Int Encoding") {
    val expected = Array[Byte](0x80.toByte)
    val data = RLPEncoding.encode(0)
    assert(expected sameElements data)
    val dataObtained = RLPEncoding.decode[Int](data)
    val obtained: Int = dataObtained
    assert(0 == obtained)

    val expected2 = Array(0x78.toByte)
    val data2 = RLPEncoding.encode(120)
    assert(expected2 sameElements data2)
    val dataObtained2 = RLPEncoding.decode[Int](data2)
    val obtained2: Int = dataObtained2
    assert(120 == obtained2)

    val expected3 = Array(0x7F.toByte)
    val data3 = RLPEncoding.encode(127)
    assert(expected3 sameElements data3)
    val dataObtained3 = RLPEncoding.decode[Int](data3)
    val obtained3: Int = dataObtained3
    assert(127 == obtained3)

    val expected4 = Array(0x82.toByte, 0x76.toByte, 0x5F.toByte)
    val data4 = RLPEncoding.encode(30303)
    assert(expected4 sameElements data4)
    val dataObtained4 = RLPEncoding.decode[Int](data4)
    val obtained4: Int = dataObtained4
    assert(30303 == obtained4)

    val expected5 = Array(0x82.toByte, 0x4E.toByte, 0xEA.toByte)
    val data5 = RLPEncoding.encode(20202)
    assert(expected5 sameElements data5)
    val dataObtained5 = RLPEncoding.decode[Int](data5)
    val obtained5: Int = dataObtained5
    assert(20202 == obtained5)

    val expected6 = Array(0x83.toByte, 1.toByte, 0.toByte, 0.toByte)
    val data6 = RLPEncoding.encode(65536)
    assert(expected6 sameElements data6)
    val dataObtained6 = RLPEncoding.decode[Int](data6)
    val obtained6: Int = dataObtained6
    assert(65536 == obtained6)

    val expected7 = Array(0x84.toByte, 0x80.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    val data7 = RLPEncoding.encode(Integer.MIN_VALUE)
    assert(expected7 sameElements data7)
    val dataObtained7 = RLPEncoding.decode[Int](data7)
    val obtained7: Int = dataObtained7
    assert(Integer.MIN_VALUE == obtained7)

    val expected8 = Array(0x84.toByte, 0x7F.toByte, 0xFF.toByte, 0xFF.toByte, 0xFF.toByte)
    val data8 = RLPEncoding.encode(Integer.MAX_VALUE)
    assert(expected8 sameElements data8)
    val dataObtained8 = RLPEncoding.decode[Int](data8)
    val obtained8: Int = dataObtained8
    assert(Integer.MAX_VALUE == obtained8)

    val expected9 = Array(0x84.toByte, 0xFF.toByte, 0xFF.toByte, 0xFF.toByte, 0xFF.toByte)
    val data9 = RLPEncoding.encode(0xFFFFFFFF)
    assert(expected9 sameElements data9)
    val dataObtained9 = RLPEncoding.decode[Int](data9)
    val obtained9: Int = dataObtained9
    assert(0xFFFFFFFF == obtained9)

    forAll(Gen.choose[Int](Int.MinValue, Int.MaxValue)) {
      (anInt: Int) => {
        val data = RLPEncoding.encode(anInt)
        val dataObtained = RLPEncoding.decode[Int](data)
        val obtained: Int = dataObtained
        assert(anInt == obtained)
      }
    }
  }

  test("Long Encoding") {
    forAll(Gen.choose[Long](0, Long.MaxValue)) {
      (aLong: Long) => {
        val data = RLPEncoding.encode(aLong)
        val dataObtained = RLPEncoding.decode[Long](data)
        val obtained: Long = dataObtained
        assert(aLong == obtained)
      }
    }
  }

  test("BigInt Encoding") {
    val expected = Array[Byte](0x80.toByte)
    val data = RLPEncoding.encode(BigInt(0))
    assert(expected sameElements data)
    val dataObtained = RLPEncoding.decode[BigInt](data)
    val obtained: BigInt = dataObtained
    assert(BigInt(0) == obtained)


    val bigInt = BigInt("100102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f", 16)
    val expected2 = "a0100102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    val data2 = RLPEncoding.encode(bigInt)
    assert(expected2 equals Hex.toHexString(data2))
    val dataObtained2 = RLPEncoding.decode[BigInt](data2)
    val obtained2: BigInt = dataObtained2
    assert(bigInt == obtained2)


    forAll(Arbitrary.arbitrary[BigInt]) {
      (aBigIntSigned: BigInt) => {
        val aBigInt = aBigIntSigned.abs
        val data = RLPEncoding.encode(aBigInt)
        val dataObtained = RLPEncoding.decode[BigInt](data)
        val obtained: BigInt = dataObtained
        assert(aBigInt == obtained)
      }
    }
  }

  test("Byte Array Encoding") {
    val byteArr = "ce73660a06626c1b3fda7b18ef7ba3ce17b6bf604f9541d3c6c654b7ae88b239407f659c78f419025d785727ed017b6add21952d7e12007373e321dbc31824ba"
    val byteArray = Hex.decode(byteArr)
    val expected = "b840" + byteArr

    val data = RLPEncoding.encode(byteArray)
    assert(expected equals Hex.toHexString(data))
    val dataObtained = RLPEncoding.decode[Array[Byte]](data)
    val obtained: Array[Byte] = dataObtained
    assert(byteArray sameElements obtained)

    val shouldBeError = Try {
      val byteArray255Elements = Array.fill(255)(0x1.toByte)
      RLPEncoding.encode(byteArray255Elements)
    }
    assert(shouldBeError.isSuccess)

    forAll(Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])) {
      (aByteList: List[Byte]) => {
        val data = RLPEncoding.encode(aByteList.toArray)
        val dataObtained = RLPEncoding.decode[Array[Byte]](data)
        val obtained: Array[Byte] = dataObtained
        assert(aByteList.toArray sameElements obtained)
      }
    }
  }

  test("Encode ByteString") {
    forAll(Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])) {
      (aByteList: List[Byte]) => {
        val byteString = ByteString(aByteList.toArray)
        val data = RLPEncoding.encode(byteString)
        val dataObtained = RLPEncoding.decode[ByteString](data)
        val obtained: ByteString = dataObtained
        assert(byteString equals obtained)
      }
    }
  }

  test("Encode Seq"){
    forAll(Gen.nonEmptyListOf(Gen.choose[Long](0, Long.MaxValue))) {
      (aLongList: List[Long]) => {
        val aLongSeq: Seq[Long] = aLongList
        val data = RLPEncoding.encode(aLongSeq)(seqEncDec())
        val dataObtained: Seq[Long] = RLPEncoding.decode[Seq[Long]](data)(seqEncDec())
        assert(aLongSeq equals dataObtained)
      }
    }
  }

  test("Encode Empty List") {
    val expected = "c0"
    val data = RLPEncoding.encode(Seq[Any]())
    assert(expected == Hex.toHexString(data))

    val dataObtained = RLPEncoding.decode[Seq[Any]](data)
    val obtained: Seq[Any] = dataObtained
    assert(obtained.isEmpty)
  }

  test("Encode Short  List") {
    val expected = "c88363617483646f67"
    val data = RLP.encode(RLPList("cat", "dog"))
    assert(expected == Hex.toHexString(data))
    val dataObtained = RLPEncoding.decode[Seq[String]](data)
    val obtained = dataObtained
    assert(Seq("cat", "dog") equals obtained)

    val expected2 = "cc83646f6783676f6483636174"
    val data2 = RLP.encode(RLPList("dog", "god", "cat"))
    assert(expected2 == Hex.toHexString(data2))
    val dataObtained2 = RLPEncoding.decode[Seq[String]](data2)
    val obtained2 = dataObtained2
    assert(Seq("dog", "god", "cat") equals obtained2)
  }

  test("Encode Long  List") {
    val list = Seq("cat", "Lorem ipsum dolor sit amet, consectetur adipisicing elit")
    val expected = "f83e83636174b8384c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e7365637465747572206164697069736963696e6720656c6974"
    val data = RLP.encode(RLPList(list.map(i => toEncodeable(i)): _*))
    assert(expected == Hex.toHexString(data))
    val dataObtained = RLPEncoding.decode[Seq[String]](data)
    val obtained = dataObtained
    assert(list equals obtained)
  }

  test("Encode multilist") {
    val expected = "cc01c48363617483646f67c102"
    val multilist1 = MultiList1(1, Seq("cat"), "dog", Seq(2))
    val data = RLPEncoding.encode(multilist1)(MultiList1.encDec)
    assert(expected == Hex.toHexString(data))
    val dataObtained = RLPEncoding.decode[MultiList1](data)
    val obtained = dataObtained
    assert(multilist1 equals obtained)

    val multilist2 = MultiList2(Seq("cat", "dog"), Seq(1, 2))
    val expected2 = "cdc88363617483646f67c20102c0"
    val data2 = RLPEncoding.encode(multilist2)(MultiList2.encDec)
    assert(expected2 == Hex.toHexString(data2))
    val dataObtained2 = RLPEncoding.decode[MultiList2](data2)
    val obtained2 = dataObtained2
    assert(multilist2 equals obtained2)
  }

  test("Encode Empty List Of List") {
    val emptyListOfList = EmptyListOfList()
    val expected = "c4c2c0c0c0"
    val data = RLPEncoding.encode(emptyListOfList)(EmptyListOfList.encDec)
    assert(expected == Hex.toHexString(data))
    val dataObtained = RLPEncoding.decode[EmptyListOfList](data)
    val obtained = dataObtained
    assert(emptyListOfList equals obtained)
  }

  test("Encode Rep Of Two List Of List") {
    val twoListOfList = RepOfTwoListOfList()
    val expected = "c7c0c1c0c3c0c1c0"
    val data = RLPEncoding.encode(twoListOfList)(RepOfTwoListOfList.encDec)
    assert(expected == Hex.toHexString(data))
    val dataObtained = RLPEncoding.decode[RepOfTwoListOfList](data)
    val obtained = dataObtained
    assert(twoListOfList equals obtained)
  }

  test("https://github.com/ethereum/tests/blob/master/rlptest.txt") {
    for (
      input: (RLPEncodeable, String) <- rlpTestData
    ) {
      val data = RLP.encode(input._1)
      assert(input._2 == Hex.toHexString(data))
      val dataObtained = RLP.rawDecode(data)
      val obtained: RLPEncodeable = dataObtained
      val encodedAgain = RLP.encode(obtained)
      assert(data sameElements encodedAgain)
    }
  }


  test("SimpleBlock encoding") {
    val tx0 = SimpleTransaction(1, "cat")
    val tx1 = SimpleTransaction(2, "dog")

    val block = SimpleBlock(127, -127: Short, "horse", 1000, Seq(tx0, tx1), Seq(1, 2))
    val data = RLPEncoding.encode(block)(SimpleBlock.encDec)
    val dataObtained = RLPEncoding.decode[SimpleBlock](data)
    val obtained: SimpleBlock = dataObtained
    assert(block equals obtained)
  }

  ignore("Performance decode") {
    val blockRaw: String = "f8cbf8c7a00000000000000000000000000000000000000000000000000000000000000000a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347940000000000000000000000000000000000000000a02f4399b08efe68945c1cf90ffe85bbe3ce978959da753f9e649f034015b8817da00000000000000000000000000000000000000000000000000000000000000000834000008080830f4240808080a004994f67dc55b09e814ab7ffc8df3686b4afb2bb53e60eae97ef043fe03fb829c0c0"
    val payload: Array[Byte] = Hex.decode(blockRaw)
    val ITERATIONS: Int = 10000000
    println("Starting " + ITERATIONS + " decoding iterations...")
    val start1: Long = System.currentTimeMillis
    (1 to ITERATIONS).foreach { _ => RLP.rawDecode(payload); Unit }
    val end1: Long = System.currentTimeMillis
    println("Result RLPEncoding.decode()\t: " + (end1 - start1) + "ms")
  }

  test("Ping message encoded using EthereumJ implementation") {
    val decoded = RLPEncoding.decode[PingMessage](
      Hex.decode("ee04d38c38352e36352e31392e32333182765f82765fd38c38352e36352e31392e32333182765f82765f845855366e")
    )(PingMessage.encDec)
    val parsed: PingMessage = decoded

    assert(parsed.version == 4)
    assert(new String(parsed.from.address) == "85.65.19.231")
    assert(parsed.from.tcpPort == 30303)
    assert(parsed.from.udpPort == 30303)

    assert(new String(parsed.to.address) == "85.65.19.231")
    assert(parsed.to.tcpPort == 30303)
    assert(parsed.to.udpPort == 30303)

    assert(parsed.expiration == 1481979502)
  }

  test("Partial Data Parse Test") {
    val hex: String = "000080c180000000000000000000000042699b1104e93abf0008be55f912c2ff"
    val data = Hex.decode(hex)
    val decoded: Seq[Int] = RLPEncoding.decode[Seq[Int]](data.splitAt(3)._2)
    assert(1 == decoded.length)
    assert(0 == decoded.head)
  }

  test("Multiple partial decode") {
    val seq1 = RLPList("cat", "dog")
    val seq2 = RLPList(23, 10, 1986)
    val seq3 = RLPList("cat", "Lorem ipsum dolor sit amet, consectetur adipisicing elit")
    val data = Seq(RLP.encode(seq1), RLP.encode(seq2), RLP.encode(seq3)).reduce(_ ++ _)

    val decoded1 = RLPEncoding.decode[Seq[String]](data)
    assert(decoded1 equals "cat" :: "dog" :: Nil)

    val secondItemIndex = RLPEncoding.nextElementIndex(data, 0)
    val decoded2 = RLPEncoding.decode[Seq[Int]](data.drop(secondItemIndex))
    assert(decoded2 equals 23 :: 10 :: 1986 :: Nil)

    val thirdItemIndex = RLPEncoding.nextElementIndex(data, secondItemIndex)
    val decoded3 = RLPEncoding.decode[Seq[String]](data.drop(thirdItemIndex))
    assert(decoded3 equals Seq("cat", "Lorem ipsum dolor sit amet, consectetur adipisicing elit"))
  }

  test("Multiple objects partial decode") {
    val multiplePingEncoded =
      Hex.decode("ee04d38c38352e36352e31392e32333182765f82765fd38c38352e36352e31392e32333182765f82765f845855366e") ++
        Hex.decode("ee04d38c38352e36352e31392e32333182765f82765fd38c38352e36352e31392e32333182765f82765f845855366e") ++
        Hex.decode("ee04d38c38352e36352e31392e32333182765f82765fd38c38352e36352e31392e32333182765f82765f845855366e")

    val result = Try {
      val decoded = RLPEncoding.decode[PingMessage](multiplePingEncoded)(PingMessage.encDec)

      val secondItemIndex = RLPEncoding.nextElementIndex(multiplePingEncoded, 0)
      val decoded2 = RLPEncoding.decode[PingMessage](multiplePingEncoded.drop(secondItemIndex))(PingMessage.encDec)

      val thirdItemIndex = RLPEncoding.nextElementIndex(multiplePingEncoded, secondItemIndex)
      val decoded3 = RLPEncoding.decode[PingMessage](multiplePingEncoded.drop(thirdItemIndex))(PingMessage.encDec)
    }
    assert(result.isSuccess)
  }

  implicit def emptySeqEncDec = new RLPEncoder[Seq[Any]] with RLPDecoder[Seq[Any]] {
    override def encode(obj: Seq[Any]): RLPEncodeable = RLPList(Seq(): _*)

    override def decode(rlp: RLPEncodeable): Seq[Any] = rlp match {
      case l: RLPList if l.items.isEmpty => Seq()
      case _ => throw new Exception("src is not an empty Seq")
    }
  }

  implicit val stringSeqEncDec = new RLPEncoder[Seq[String]] with RLPDecoder[Seq[String]] {
    override def encode(strings: Seq[String]): RLPEncodeable = RLPList(strings.map(stringEncDec.encode): _*)

    override def decode(rlp: RLPEncodeable): Seq[String] = rlp match {
      case l: RLPList => l.items.map(item => item: String)
      case _ => throw new RuntimeException("Invalid String Seq Decoder")
    }
  }

  implicit def stringSeqFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[Seq[String]]): Seq[String] = dec.decode(rlp)

  implicit val intSeqEncDec = new RLPEncoder[Seq[Int]] with RLPDecoder[Seq[Int]] {
    override def encode(ints: Seq[Int]): RLPEncodeable = ints: RLPList

    override def decode(rlp: RLPEncodeable): Seq[Int] = rlp match {
      case l: RLPList => l.items.map(item => item: Int)
      case _ => throw new RuntimeException("Invalid Int Seq Decoder")
    }
  }

  implicit def intSeqFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[Seq[Int]]): Seq[Int] = dec.decode(rlp)

  case class MultiList1(number: Int, seq1: Seq[String], string: String, seq2: Seq[Int])

  object MultiList1 {
    implicit val encDec = new RLPEncoder[MultiList1] with RLPDecoder[MultiList1] {
      override def encode(obj: MultiList1): RLPEncodeable = {
        import obj._
        RLPList(number, seq1, string, seq2)
      }

      override def decode(rlp: RLPEncodeable): MultiList1 = rlp match {
        case l: RLPList => MultiList1(l.items.head, l.items(1), l.items(2), l.items(3))
        case _ => throw new RuntimeException("Invalid Int Seq Decoder")
      }
    }
  }

  case class MultiList2(seq1: Seq[String], seq2: Seq[Int], seq3: Seq[Any] = Seq())

  object MultiList2 {
    implicit val encDec = new RLPEncoder[MultiList2] with RLPDecoder[MultiList2] {
      override def encode(obj: MultiList2): RLPEncodeable = {
        import obj._
        RLPList(seq1, seq2, seq3)
      }

      override def decode(rlp: RLPEncodeable): MultiList2 = rlp match {
        case l: RLPList => MultiList2(l.items.head, l.items(1), emptySeqEncDec.decode(l.items(2)))
        case _ => throw new RuntimeException("Invalid Int Seq Decoder")
      }
    }
  }

  case class EmptyListOfList()

  object EmptyListOfList {
    val instance = Seq(RLPList(RLPList(), RLPList()), RLPList())

    implicit val encDec = new RLPEncoder[EmptyListOfList] with RLPDecoder[EmptyListOfList] {
      override def encode(obj: EmptyListOfList): RLPEncodeable = RLPList(instance: _*)

      override def decode(rlp: RLPEncodeable): EmptyListOfList = rlp match {
        case l: RLPList =>
          l.items match {
            case items if items == instance => EmptyListOfList()
            case _ => throw new RuntimeException("Invalid EmptyListOfList Decoder")
          }
        case _ => throw new RuntimeException("Invalid EmptyListOfList Decoder")
      }
    }
  }

  case class RepOfTwoListOfList()

  object RepOfTwoListOfList {
    val instance = Seq(RLPList(), RLPList(RLPList()), RLPList(RLPList(), RLPList(RLPList())))

    implicit val encDec = new RLPEncoder[RepOfTwoListOfList] with RLPDecoder[RepOfTwoListOfList] {
      override def encode(obj: RepOfTwoListOfList): RLPEncodeable = RLPList(instance: _*)

      override def decode(rlp: RLPEncodeable): RepOfTwoListOfList = rlp match {
        case l: RLPList =>
          l.items match {
            case items if items == instance => RepOfTwoListOfList()
            case _ => throw new RuntimeException("Invalid RepOfTwoListOfList Decoder")
          }
        case _ => throw new RuntimeException("Invalid RepOfTwoListOfList Decoder")
      }
    }
  }

  val rlpTestData: Seq[(RLPEncodeable, String)] = Seq(
    intEncDec.encode(0) -> "80",
    stringEncDec.encode("") -> "80",
    stringEncDec.encode("d") -> "64",
    stringEncDec.encode("cat") -> "83636174",
    stringEncDec.encode("dog") -> "83646f67",
    stringSeqEncDec.encode(Seq("cat", "dog")) -> "c88363617483646f67",
    stringSeqEncDec.encode(Seq("dog", "god", "cat")) -> "cc83646f6783676f6483636174",
    intEncDec.encode(1) -> "01",
    intEncDec.encode(10) -> "0a",
    intEncDec.encode(100) -> "64",
    intEncDec.encode(1000) -> "8203e8",
    bigIntEncDec.encode(BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639935"))
      -> "a0ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    bigIntEncDec.encode(BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639936"))
      -> "a1010000000000000000000000000000000000000000000000000000000000000000"
  )

  case class SimpleTransaction(id: Int, name: String)

  object SimpleTransaction {
    implicit val encDec = new RLPEncoder[SimpleTransaction] with RLPDecoder[SimpleTransaction] {
      override def encode(obj: SimpleTransaction): RLPEncodeable = {
        import obj._
        RLPList(id, name)
      }

      override def decode(rlp: RLPEncodeable): SimpleTransaction = rlp match {
        case RLPList(id, name) => SimpleTransaction(id, name)
        case _ => throw new RuntimeException("Invalid Simple Transaction")
      }
    }

    implicit def fromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[SimpleTransaction]): SimpleTransaction = dec.decode(rlp)
  }

  case class SimpleBlock(id: Byte, parentId: Short, owner: String, nonce: Int, txs: Seq[SimpleTransaction], unclesIds: Seq[Int])

  object SimpleBlock {
    implicit val encDec = new RLPEncoder[SimpleBlock] with RLPDecoder[SimpleBlock] {
      override def encode(obj: SimpleBlock): RLPEncodeable = {
        import obj._
        RLPList(id, parentId, owner, nonce, RLPList(txs.map(SimpleTransaction.encDec.encode): _*), RLPList(unclesIds.map(id => id: RLPEncodeable): _*))
      }

      override def decode(rlp: RLPEncodeable): SimpleBlock = rlp match {
        case RLPList(id, parentId, owner, nonce, (txs: RLPList), (unclesIds: RLPList)) =>
          SimpleBlock(id, parentId, owner, nonce, txs.items.map(SimpleTransaction.encDec.decode), unclesIds.items.map(intEncDec.decode))
        case _ => throw new Exception("Can't transform RLPEncodeable to block")
      }
    }
  }

  // FIXME this a port from EthereumJ Ping message in order to check parsing, once we define the this entities in
  // our codebase, this should be removed
  case class Endpoint(address: Array[Byte], tcpPort: Long, udpPort: Long)

  case class PingMessage(version: Int, from: Endpoint, to: Endpoint, expiration: Long)

  object Endpoint {
    implicit val encDec = new RLPEncoder[Endpoint] with RLPDecoder[Endpoint] {
      override def encode(obj: Endpoint): RLPEncodeable = {
        import obj._
        RLPList(address, tcpPort, udpPort)
      }

      override def decode(rlp: RLPEncodeable): Endpoint = rlp match {
        case l: RLPList => Endpoint(l.items.head, l.items(1), l.items(2))
        case _ => throw new RuntimeException("Invalid Endpoint")
      }
    }

    implicit def endpointFromEncodeable(rlp: RLPEncodeable)(implicit dec: RLPDecoder[Endpoint]): Endpoint = dec.decode(rlp)
  }

  object PingMessage {

    implicit val encDec = new RLPEncoder[PingMessage] with RLPDecoder[PingMessage] {
      override def encode(obj: PingMessage): RLPEncodeable = {
        import obj._
        RLPList(version, from, to, expiration)
      }

      override def decode(rlp: RLPEncodeable): PingMessage = rlp match {
        case l: RLPList => PingMessage(l.items.head, l.items(1), l.items(2), l.items(3))
        case _ => throw new RuntimeException("Not a Ping Message")
      }
    }

  }

}

