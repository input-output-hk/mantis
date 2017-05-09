package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import org.json4s.JsonAST.{JNull, JString}
import org.json4s.{CustomSerializer, DefaultFormats, Extraction, JValue}
import org.spongycastle.util.encoders.Hex

object JsonSerializers {

  object UnformattedDataJsonSerializer extends CustomSerializer[ByteString](_ =>
    (
      { PartialFunction.empty },
      { case bs: ByteString => JString(s"0x${Hex.toHexString(bs.toArray)}") }
    )
  )

  object QuantitiesSerializer extends CustomSerializer[BigInt](_ =>
    (
      {PartialFunction.empty},
      {
        case n: BigInt =>
          val res = if(n == 0)
            JString("0x0")
          else
            JString(s"0x${Hex.toHexString(n.toByteArray).dropWhile(_ == '0')}")
          res
      }
    )
  )

  object OptionToJNullSerializer extends CustomSerializer[Option[_]](formats =>
    (
      {PartialFunction.empty},
      { case None => JNull }
    )
  )

  object SignedTransactionViewSerializer extends CustomSerializer[TransactionResponse](_ =>
    (
      {PartialFunction.empty},
      { case stxView: TransactionResponse =>
          implicit val formats = DefaultFormats.preservingEmptyValues +
            UnformattedDataJsonSerializer +
            OptionToJNullSerializer

          Extraction.decompose(stxView)
      }
    )
  )

  object BlockViewSerializer extends CustomSerializer[BlockResponse](_ =>
    (
      {PartialFunction.empty},
      { case blockView: BlockResponse =>
          implicit val formats = DefaultFormats.preservingEmptyValues +
            UnformattedDataJsonSerializer +
            OptionToJNullSerializer +
            SignedTransactionViewSerializer +
            QuantitiesSerializer

          Extraction.decompose(blockView)
      }
    )
  )

}
