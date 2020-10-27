package io.iohk.ethereum.forking

import akka.util.ByteString
import cats.implicits._
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder, HCursor, Json}
import io.iohk.ethereum.domain.{Address, UInt256}
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import io.iohk.ethereum.mallet.common.StringUtil
import io.iohk.ethereum.utils.ByteStringUtils

import scala.util.Try

object JsonCodecs {
  private val hexPrefix = "0x"

  implicit val byteStringDecoder: Decoder[ByteString] =
    (c: HCursor) =>
      c.as[String]
        .map(StringUtil.drop0x)
        .map(ByteStringUtils.string2hash)
  implicit val byteStringEncoder: Encoder[ByteString] =
    (value: ByteString) => (hexPrefix + ByteStringUtils.hash2string(value)).asJson

  implicit val bigIntEncoder: Encoder[BigInt] = Encoder[String].contramap(number => hexPrefix + number.toString(16))
  implicit val bigIntDecoder: Decoder[BigInt] =
    (c: HCursor) =>
      c.as[String]
        .map(StringUtil.hexToBigInt)

  implicit val uint256Decoder: Decoder[UInt256] =
    Decoder[BigInt].emap(bigInt => Try { UInt256(bigInt) }.toEither.leftMap(_ => "Couldn't parse as UInt256"))
  implicit val uint256Encoder: Encoder[UInt256] = Encoder[BigInt].contramap(_.toBigInt)

  implicit val blockParamEncoder: Encoder[BlockParam] = {
    case BlockParam.WithNumber(n: BigInt) => n.asJson
    case BlockParam.Latest => Json.fromString("latest")
    case BlockParam.Pending => Json.fromString("pending")
    case BlockParam.Earliest => Json.fromString("earliest")
  }

  implicit val addressDecoder: Decoder[Address] = Decoder[ByteString].emapTry(bs => Try { Address(bs) })
  implicit val addressEncoder: Encoder[Address] = Encoder[ByteString].contramap(_.bytes)
}
