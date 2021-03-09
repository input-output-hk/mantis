package io.iohk.ethereum.jsonrpc.serialization

import io.iohk.ethereum.jsonrpc.JsonMethodsImplicits
import org.json4s.JArray
import org.json4s.JBool
import org.json4s.JInt
import org.json4s.JLong
import org.json4s.JNull
import org.json4s.JString
import org.json4s.JValue

trait JsonEncoder[T] {
  def encodeJson(t: T): JValue
}
object JsonEncoder {
  def apply[T](implicit encoder: JsonEncoder[T]): JsonEncoder[T] = encoder

  def encode[T](value: T)(implicit encoder: JsonEncoder[T]): JValue = encoder.encodeJson(value)

  object Ops {
    implicit class JsonEncoderOps[T](val item: T) extends AnyVal {
      def jsonEncoded(implicit encoder: JsonEncoder[T]): JValue = encoder.encodeJson(item)
    }
  }

  implicit val stringEncoder: JsonEncoder[String] = JString(_)
  implicit val intEncoder: JsonEncoder[Int] = JInt(_)
  implicit val longEncoder: JsonEncoder[Long] = JLong(_)
  implicit val booleanEncoder: JsonEncoder[Boolean] = JBool(_)
  implicit val jvalueEncoder: JsonEncoder[JValue] = identity
  implicit val bigIntEncoder: JsonEncoder[BigInt] = JsonMethodsImplicits.encodeAsHex(_)

  implicit def listEncoder[T](implicit itemEncoder: JsonEncoder[T]): JsonEncoder[List[T]] = list =>
    JArray(list.map(itemEncoder.encodeJson))

  trait OptionToNull {
    implicit def optionToNullEncoder[T](implicit valueEncoder: JsonEncoder[T]): JsonEncoder[Option[T]] = {
      case Some(value) => valueEncoder.encodeJson(value)
      case None        => JNull
    }
  }
  object OptionToNull extends OptionToNull
}
