package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.PersonalService.{InvalidAddress, SendIeleTransactionRequest}
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodDecoder}
import org.json4s.JsonAST.{JArray, JObject, JString, JValue}

object IeleJsonMethodsImplicits extends JsonMethodsImplicits {

  def extractIeleCall(obj: JObject): Either[JsonRpcError, IeleCallTx] = {
    def toEitherOpt[A, B](opt: Option[Either[A, B]]): Either[A, Option[B]] =
      opt.map(_.map(Some.apply)).getOrElse(Right(None))

    for {
      from <- toEitherOpt((obj \ "from").extractOpt[String].map(extractBytes))
      to <- toEitherOpt((obj \ "to").extractOpt[String].map(extractBytes))
      gas <- optionalQuantity(obj \ "gas")
      gasPrice <- optionalQuantity(obj \ "gasPrice")
      value <- optionalQuantity(obj \ "value")
      function = (obj \ "function").extractOpt[String]
      contractCode <- toEitherOpt((obj \ "contractCode").extractOpt[String].map(extractBytes))
      arguments = (obj \ "arguments").extractOpt[Seq[String]].map(_.map(in => ByteString(decode(in))))
    } yield IeleCallTx(
      from = from,
      to = to,
      gas = gas,
      gasPrice = gasPrice.getOrElse(0),
      value = value.getOrElse(0),
      function = function,
      contractCode = contractCode,
      arguments = arguments
    )
  }

  implicit val iele_call = new JsonMethodDecoder[IeleCallRequest] with JsonEncoder[IeleCallResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, IeleCallRequest] =
      params match {
        case Some(JArray((txObj: JObject) :: (blockValue: JValue) :: Nil)) =>
          for {
            blockParam <- extractBlockParam(blockValue)
            tx <- extractIeleCall(txObj)
          } yield IeleCallRequest(tx, blockParam)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: IeleCallResponse): JValue = JArray(t.returnData.map(encodeAsHex).toList)
  }

  protected def extractIeleTx(input: Map[String, JValue]): Either[JsonRpcError, IeleTransactionRequest] = {
    def optionalQuantity(name: String): Either[JsonRpcError, Option[BigInt]] = input.get(name) match {
      case Some(v) => extractQuantity(v).map(Some(_))
      case None => Right(None)
    }

    for {
      from <- input.get("from") match {
        case Some(JString(s)) => extractAddress(s)
        case Some(_) => Left(InvalidAddress)
        case _ => Left(InvalidParams("TX 'from' is required"))
      }

      to <- input.get("to") match {
        case Some(JString(s)) =>
          extractAddress(s).map {
            case addr if addr.toUInt256.isZero => None
            case addr => Some(addr)
          }

        case Some(_) => Left(InvalidAddress)
        case None => Right(None)
      }

      value <- optionalQuantity("value")

      gas <- optionalQuantity("gas")

      gasPrice <- optionalQuantity("gasPrice")

      nonce <- optionalQuantity("nonce")

      function = input.get("function").flatMap(_.extractOpt[String])

      arguments = input.get("arguments").flatMap(_.extractOpt[Seq[String]].map(_.map(in => ByteString(decode(in)))))

      contractCode <- input.get("contractCode") match {
        case Some(JString(s)) => extractBytes(s).map(Some(_))
        case Some(_) => Left(InvalidParams())
        case None => Right(None)
      }

    } yield IeleTransactionRequest(from, to, value, gas, gasPrice, nonce, function, arguments, contractCode)
  }

  implicit val iele_sendTransaction = new JsonMethodDecoder[SendIeleTransactionRequest] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SendIeleTransactionRequest] =
      params match {
        case Some(JArray(JObject(tx) :: _)) =>
          extractIeleTx(tx.toMap).map(SendIeleTransactionRequest)
        case _ =>
          Left(InvalidParams())
      }
  }

}
