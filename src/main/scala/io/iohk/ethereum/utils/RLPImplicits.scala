package io.iohk.ethereum.utils

import io.iohk.ethereum.utils.RLP._

import scala.language.implicitConversions

object RLPImplicits {

  implicit def byteToRLPValue(singleByte: Byte): RLPValue = new RLPValue {
    override def toBytes: Array[Byte] = encodeByte(singleByte)
  }

  implicit def shortToRLPValue(singleShort: Short): RLPValue = new RLPValue {
    override def toBytes: Array[Byte] = encodeShort(singleShort)
  }

  implicit def intToRLPValue(singleInt: Int): RLPValue = new RLPValue {
    override def toBytes: Array[Byte] = encodeInt(singleInt)
  }

  implicit def longToRLPValue(singleLong: Long): RLPValue = bigIntToRLPValue(BigInt(singleLong))

  implicit def stringToRLPValue(srcString: String): RLPValue = new RLPValue {
    override def toBytes: Array[Byte] = srcString.getBytes()
  }

  implicit def bigIntToRLPValue(srcBigInteger: BigInt): RLPValue = new RLPValue {

    def asUnsignedByteArray(srcBigInteger: BigInt): Array[Byte] = {
      val asByteArray = srcBigInteger.toByteArray
      if (asByteArray(0) == 0) asByteArray.tail
      else asByteArray
    }

    override def toBytes: Array[Byte] = {
      if (srcBigInteger.equals(BigInt(0))) encodeByte(0: Byte)
      else asUnsignedByteArray(srcBigInteger)
    }
  }

  implicit def byteArrayToRLPValue(srcByteArray: Array[Byte]): RLPValue = new RLPValue {
    override def toBytes: Array[Byte] = srcByteArray
  }

  implicit def rlpEncodeableToByte(srcRLPEncodeable: RLPEncodeable): Byte = {
    srcRLPEncodeable match{
      case srcRLPValue: RLPValue =>
        val bytes = srcRLPValue.toBytes
        bytes.length match{
          case 0 => 0: Byte
          case 1 => (bytes(0) & 0xFF).toByte
          case _ => throw new Exception("src doesn't represent a byte")
        }
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit def rlpEncodeableToShort(srcRLPEncodeable: RLPEncodeable): Short = {
    srcRLPEncodeable match{
      case srcRLPValue: RLPValue =>
        val bytes = srcRLPValue.toBytes
        bytes.length match{
          case 0 => 0: Short
          case 1 => (bytes(0) & 0xFF).toShort
          case 2 => (((bytes(0) & 0xFF) << 8) + (bytes(1) & 0xFF)).toShort
          case _ => throw new Exception("src doesn't represent a short")
        }
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit def rlpEncodeableToInt(srcRLPEncodeable: RLPEncodeable): Int = {
    srcRLPEncodeable match{
      case srcRLPValue: RLPValue => decodeInt(srcRLPValue.toBytes)
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit def rlpEncodeableToLong(srcRLPEncodeable: RLPEncodeable): Long = rlpEncodeableToBigInteger(srcRLPEncodeable).toLong

  implicit def rlpEncodeableToString(srcString: RLPEncodeable): String = {
    srcString match{
      case srcString: RLPValue => new String(srcString.toBytes)
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit def rlpEncodeableToBigInteger(srcRLPEncodeable: RLPEncodeable): BigInt = {
    srcRLPEncodeable match{
      case srcRLPValue: RLPValue => srcRLPValue.toBytes.foldLeft[BigInt](BigInt(0)){(rec, byte) => (rec << (8: Int)) + BigInt(byte & 0xFF) }
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit def rlpEncodeableToArrayByte(srcRLPEncodeable: RLPEncodeable): Array[Byte] = {
    srcRLPEncodeable match{
      case srcRLPValue: RLPValue => srcRLPValue.toBytes
      case _ => throw new Exception("src is not an RLPValue")
    }
  }

  implicit def rlpEncodeableToRLPList(srcRLPEncodeable: RLPEncodeable): RLPList = {
    srcRLPEncodeable match{
      case srcRLPList: RLPList => srcRLPList
      case _ => throw new Exception("src is not an RLPList")
    }
  }
}
