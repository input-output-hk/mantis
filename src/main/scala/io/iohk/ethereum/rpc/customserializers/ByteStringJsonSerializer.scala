package io.iohk.ethereum.rpc.customserializers

import akka.util.ByteString
import org.spongycastle.util.encoders.Hex
import spray.json.{DefaultJsonProtocol, JsString, JsValue, RootJsonFormat}

/**
  * Custom Serializer to encode [[akka.util.ByteString]] in Hex format with leading 0x
  */
object ByteStringJsonSerializer extends DefaultJsonProtocol {

  val HexPrefix = "0x"

  implicit object ByteStringJsonFormat extends RootJsonFormat[ByteString] {

    override def write(obj: ByteString): JsValue = JsString(s"$HexPrefix${Hex.toHexString(obj.toArray[Byte])}")

    override def read(json: JsValue): ByteString = {
      json match {
        case JsString(value: String) if value startsWith HexPrefix => ByteString(Hex.decode(value.substring(HexPrefix.length)))
        case _ => ???
      }
    }
  }
}
