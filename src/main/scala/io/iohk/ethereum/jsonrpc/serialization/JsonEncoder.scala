package io.iohk.ethereum.jsonrpc.serialization

import org.json4s.{JArray, JBool, JInt, JNull, JString, JValue}

trait JsonEncoder[T] {
  def encodeJson(t: T): JValue
}
object JsonEncoder {
  def apply[T](implicit encoder: JsonEncoder[T]): JsonEncoder[T] = encoder

  def encode[T](value: T)(implicit encoder: JsonEncoder[T]): JValue = encoder.encodeJson(value)

  implicit val stringEncoder: JsonEncoder[String] = JString(_)
  implicit val intEncoder: JsonEncoder[Int] = JInt(_)
  implicit val booleanEncoder: JsonEncoder[Boolean] = JBool(_)
  implicit val jvalueEncoder: JsonEncoder[JValue] = identity

  implicit def listEncoder[T](implicit itemEncoder: JsonEncoder[T]): JsonEncoder[List[T]] = list =>
    JArray(list.map(itemEncoder.encodeJson))

  trait OptionToNull {
    implicit def optionToNullEncoder[T](implicit valueEncoder: JsonEncoder[T]): JsonEncoder[Option[T]] = {
      case Some(value) => valueEncoder.encodeJson(value)
      case None => JNull
    }
  }
  object OptionToNull extends OptionToNull
}
