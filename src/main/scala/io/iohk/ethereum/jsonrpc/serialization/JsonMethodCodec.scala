package io.iohk.ethereum.jsonrpc.serialization
import org.json4s.JArray

trait JsonMethodCodec[Req, Res] extends JsonMethodDecoder[Req] with JsonEncoder[Res]
object JsonMethodCodec {
  import scala.language.implicitConversions

  implicit def decoderWithEncoderIntoCodec[Req, Res](
      decEnc: JsonMethodDecoder[Req] with JsonEncoder[Res]
  ): JsonMethodCodec[Req, Res] = new JsonMethodCodec[Req, Res] {
    def decodeJson(params: Option[JArray]) = decEnc.decodeJson(params)
    def encodeJson(t: Res) = decEnc.encodeJson(t)
  }
}
