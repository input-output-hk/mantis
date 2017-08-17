package io.iohk.ethereum.ets.blockchain

import akka.util.ByteString
import io.circe._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.NumericUtils.parseHexOrDecNumber
import io.iohk.ethereum.vm.UInt256
import org.spongycastle.util.encoders.Hex

import scala.util.Try

trait Decoders {
   implicit val decodeBigInt: Decoder[BigInt] = {
    (c: HCursor) =>
      // try converting from JSON number
      c.as[JsonNumber].flatMap(n => Try(n.toBigInt.get).toEither).left.flatMap { _ =>
        // if that fails, convert from JSON string
        c.as[String].flatMap(stringToBigInt)
          .left.map(DecodingFailure.fromThrowable(_, c.history))
      }
  }

   implicit val decodeLong: Decoder[Long] =
    decodeBigInt.map(_.toLong)

   implicit val decodeUInt256: Decoder[UInt256] =
    decodeBigInt.map(UInt256(_))

   implicit val decodeBigIntKey: KeyDecoder[BigInt] =
    (key: String) => stringToBigInt(key).toOption

   implicit val decodeUInt256Key: KeyDecoder[UInt256] =
    decodeBigIntKey.map(UInt256(_))

   implicit val decodeByteString: Decoder[ByteString] =
    (c: HCursor) => c.as[String].map(s => ByteString(decodeString(s)))

   implicit val decodeAddress: Decoder[Address] =
    (c: HCursor) => c.as[String].map(Address(_))

   implicit val decodeOptionalAddress: Decoder[Option[Address]] =
    (c: HCursor) => c.as[String].map { s =>
      if (s.isEmpty) None else Some(Address(s))
    }

   implicit val decodeAddressKey: KeyDecoder[Address] =
    (key: String) => Some(Address(key))

  private def decodeString(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    val normalized = if (stripped.length % 2 == 1) "0" + stripped else stripped
    Hex.decode(normalized)
  }

   def stringToBigInt(s: String): Either[Throwable, BigInt] =
    if (s.isEmpty || s == "0x") Right(BigInt(0)) else Try(parseHexOrDecNumber(s)).toEither
}
