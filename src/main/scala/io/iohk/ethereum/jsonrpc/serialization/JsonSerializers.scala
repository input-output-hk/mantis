package io.iohk.ethereum.jsonrpc.serialization

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.JsonRpcError
import org.bouncycastle.util.encoders.Hex
import org.json4s.{CustomSerializer, DefaultFormats, Formats, JNull, JString}

object JsonSerializers {
  implicit val formats: Formats =
    DefaultFormats + UnformattedDataJsonSerializer + QuantitiesSerializer + OptionNoneToJNullSerializer + AddressJsonSerializer

  object UnformattedDataJsonSerializer
      extends CustomSerializer[ByteString](_ =>
        (
          PartialFunction.empty,
          { case bs: ByteString => JString(s"0x${Hex.toHexString(bs.toArray)}") }
        )
      )

  object QuantitiesSerializer
      extends CustomSerializer[BigInt](_ =>
        (
          PartialFunction.empty,
          { case n: BigInt =>
            if (n == 0)
              JString("0x0")
            else
              JString(s"0x${Hex.toHexString(n.toByteArray).dropWhile(_ == '0')}")
          }
        )
      )

  object OptionNoneToJNullSerializer
      extends CustomSerializer[Option[_]](formats =>
        (
          PartialFunction.empty,
          { case None => JNull }
        )
      )

  object AddressJsonSerializer
      extends CustomSerializer[Address](_ =>
        (
          PartialFunction.empty,
          { case addr: Address => JString(s"0x${Hex.toHexString(addr.bytes.toArray)}") }
        )
      )

  object RpcErrorJsonSerializer
      extends CustomSerializer[JsonRpcError](_ =>
        (
          PartialFunction.empty,
          { case err: JsonRpcError => JsonEncoder.encode(err) }
        )
      )

}
