package io.iohk.ethereum.jsonrpc.client

import akka.util.ByteString
import io.circe._
import io.circe.syntax._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.NumericUtils._
import io.iohk.ethereum.utils.StringUtils
import org.bouncycastle.util.encoders.Hex

import scala.util.Try

object CommonJsonCodecs {
  implicit val decodeBigInt: Decoder[BigInt] = { (c: HCursor) =>
    // try converting from JSON number
    c.as[JsonNumber].flatMap(n => Try(n.toBigInt.get).toEither).left.flatMap { _ =>
      // if that fails, convert from JSON string
      c.as[String].flatMap(stringToBigInt).left.map(DecodingFailure.fromThrowable(_, c.history))
    }
  }

  implicit val encodeByteString: Encoder[ByteString] =
    (b: ByteString) => ("0x" + Hex.toHexString(b.toArray)).asJson

  implicit val decodeByteString: Decoder[ByteString] =
    (c: HCursor) => c.as[String].map(s => ByteString(Hex.decode(StringUtils.drop0x(s))))

  implicit val encodeAddress: Encoder[Address] =
    (a: Address) => a.toString.asJson

  implicit val decodeAddress: Decoder[Address] =
    (c: HCursor) => c.as[String].map(Address(_))

  private def stringToBigInt(s: String): Either[Throwable, BigInt] =
    if (s.isEmpty || s == "0x") Right(BigInt(0)) else Try(parseHexOrDecNumber(s)).toEither
}
