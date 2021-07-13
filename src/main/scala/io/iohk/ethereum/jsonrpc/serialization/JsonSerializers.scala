package io.iohk.ethereum.jsonrpc.serialization

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex
import org.json4s.CustomSerializer
import org.json4s.DefaultFormats
import org.json4s.Extraction
import org.json4s.Formats
import org.json4s.JString

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.JsonRpcError
import io.iohk.ethereum.testmode.EthTransactionResponse

object JsonSerializers {
  implicit val formats: Formats =
    DefaultFormats + UnformattedDataJsonSerializer + QuantitiesSerializer + AddressJsonSerializer + EthTransactionResponseSerializer

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

  /** Specific EthTransactionResponse serializer.
    * It's purpose is to encode the optional "to" field, as requested by
    * retesteth
    */
  object EthTransactionResponseSerializer
      extends CustomSerializer[EthTransactionResponse](_ =>
        (
          PartialFunction.empty,
          { case tx: EthTransactionResponse =>
            implicit val formats =
              DefaultFormats.preservingEmptyValues + UnformattedDataJsonSerializer + QuantitiesSerializer + AddressJsonSerializer
            Extraction.decompose(tx)
          }
        )
      )
}
