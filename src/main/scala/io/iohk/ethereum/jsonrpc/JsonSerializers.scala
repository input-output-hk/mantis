package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import org.json4s.JsonAST.{JNull, JString}
import org.json4s.{CustomSerializer, DefaultFormats, Extraction, JValue}
import org.spongycastle.util.encoders.Hex

object JsonSerializers {

  object ByteStringJsonSerializer extends CustomSerializer[ByteString](formats =>
    (
      { PartialFunction.empty },
      { case bs: ByteString => serializeByteString(bs) }
    )
  )

  object OptionToJNullSerializer extends CustomSerializer[Option[_]](formats =>
    (
      {PartialFunction.empty},
      { case None => JNull }
    )
  )

  object SignedTransactionViewSerializer extends CustomSerializer[SignedTransactionView](formats =>
    (
      {PartialFunction.empty},
      { case stxView: SignedTransactionView =>
          implicit val formats = DefaultFormats.preservingEmptyValues +
            ByteStringJsonSerializer +
            OptionToJNullSerializer

          Extraction.decompose(stxView)
      }
    )
  )

  private def serializeByteString(bs: ByteString): JValue =
    JString(s"0x${Hex.toHexString(bs.toArray[Byte])}")

}
