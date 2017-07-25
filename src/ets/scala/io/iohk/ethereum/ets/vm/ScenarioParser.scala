package io.iohk.ethereum.ets.vm

import akka.util.ByteString
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.NumericUtils._
import io.iohk.ethereum.vm.UInt256
import org.spongycastle.util.encoders.Hex

import scala.util.Try

//TODO: better error handling
object ScenarioParser {

  private implicit val decodeBigInt: Decoder[BigInt] = {
    (c: HCursor) =>
      // try converting from JSON number
      c.as[JsonNumber].flatMap(n => Try(n.toBigInt.get).toEither).left.flatMap { _ =>
        // if that fails, convert from JSON string
        c.as[String].flatMap(stringToBigInt)
          .left.map(DecodingFailure.fromThrowable(_, c.history))
      }
  }

  private implicit val decodeLong: Decoder[Long] =
    decodeBigInt.map(_.toLong)

  private implicit val decodeUInt256: Decoder[UInt256] =
    decodeBigInt.map(UInt256(_))

  private implicit val decodeBigIntKey: KeyDecoder[BigInt] =
    (key: String) => stringToBigInt(key).toOption

  private implicit val decodeUInt256Key: KeyDecoder[UInt256] =
    decodeBigIntKey.map(UInt256(_))

  private implicit val decodeByteString: Decoder[ByteString] =
    (c: HCursor) => c.as[String].map(s => ByteString(Hex.decode(s.replaceFirst("^0x", ""))))

  private implicit val decodeAddress: Decoder[Address] =
    (c: HCursor) => c.as[String].map(Address(_))

  private implicit val decodeAddressKey: KeyDecoder[Address] =
    (key: String) => Some(Address(key))

  private def stringToBigInt(s: String): Either[Throwable, BigInt] =
    if (s.isEmpty || s == "0x") Right(BigInt(0)) else Try(parseHexOrDecNumber(s)).toEither


  def parse(json: String): Map[String, Scenario] =
    decode[Map[String, Scenario]](json).fold(throw _, identity)
}
