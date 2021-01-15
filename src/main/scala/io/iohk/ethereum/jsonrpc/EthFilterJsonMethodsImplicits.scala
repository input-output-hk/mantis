package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.EthFilterService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder.OptionToNull._
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.Extraction
import akka.util.ByteString

object EthFilterJsonMethodsImplicits extends JsonMethodsImplicits {
  implicit val newFilterResponseEnc = new JsonEncoder[NewFilterResponse] {
    def encodeJson(t: NewFilterResponse): JValue = encodeAsHex(t.filterId)
  }

  implicit val eth_newFilter = new JsonMethodDecoder[NewFilterRequest] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, NewFilterRequest] =
      params match {
        case Some(JArray((filterObj: JObject) :: Nil)) =>
          for {
            filter <- extractFilter(filterObj)
          } yield NewFilterRequest(filter)
        case _ => Left(InvalidParams())
      }
  }

  implicit val eth_newBlockFilter = new NoParamsMethodDecoder(NewBlockFilterRequest()) {}

  implicit val eth_newPendingTransactionFilter = new NoParamsMethodDecoder(NewPendingTransactionFilterRequest()) {}

  implicit val eth_uninstallFilter = new JsonMethodDecoder[UninstallFilterRequest]
    with JsonEncoder[UninstallFilterResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, UninstallFilterRequest] =
      params match {
        case Some(JArray((rawFilterId: JValue) :: Nil)) =>
          for {
            filterId <- extractQuantity(rawFilterId)
          } yield UninstallFilterRequest(filterId)
        case _ => Left(InvalidParams())
      }
    override def encodeJson(t: UninstallFilterResponse): JValue = JBool(t.success)
  }

  implicit val eth_getFilterChanges = new JsonMethodDecoder[GetFilterChangesRequest]
    with JsonEncoder[GetFilterChangesResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetFilterChangesRequest] =
      params match {
        case Some(JArray((rawFilterId: JValue) :: Nil)) =>
          for {
            filterId <- extractQuantity(rawFilterId)
          } yield GetFilterChangesRequest(filterId)
        case _ => Left(InvalidParams())
      }
    override def encodeJson(t: GetFilterChangesResponse): JValue =
      t.filterChanges match {
        case FilterManager.LogFilterChanges(logs) => JArray(logs.map(Extraction.decompose).toList)
        case FilterManager.BlockFilterChanges(blockHashes) => JArray(blockHashes.map(encodeAsHex).toList)
        case FilterManager.PendingTransactionFilterChanges(txHashes) => JArray(txHashes.map(encodeAsHex).toList)
      }
  }

  implicit val eth_getFilterLogs = new JsonMethodDecoder[GetFilterLogsRequest] with JsonEncoder[GetFilterLogsResponse] {
    import FilterManager._

    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetFilterLogsRequest] =
      params match {
        case Some(JArray((rawFilterId: JValue) :: Nil)) =>
          for {
            filterId <- extractQuantity(rawFilterId)
          } yield GetFilterLogsRequest(filterId)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: GetFilterLogsResponse): JValue =
      t.filterLogs match {
        case LogFilterLogs(logs) => JArray(logs.map(Extraction.decompose).toList)
        case BlockFilterLogs(blockHashes) => JArray(blockHashes.map(encodeAsHex).toList)
        case PendingTransactionFilterLogs(txHashes) => JArray(txHashes.map(encodeAsHex).toList)
      }
  }

  implicit val eth_getLogs = new JsonMethodDecoder[GetLogsRequest] with JsonEncoder[GetLogsResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetLogsRequest] =
      params match {
        case Some(JArray((filterObj: JObject) :: Nil)) =>
          for {
            filter <- extractFilter(filterObj)
          } yield GetLogsRequest(filter)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: GetLogsResponse): JValue =
      JArray(t.filterLogs.logs.map(Extraction.decompose).toList)
  }

  private def extractFilter(obj: JObject): Either[JsonRpcError, Filter] = {
    def allSuccess[T](eithers: Seq[Either[JsonRpcError, T]]): Either[JsonRpcError, Seq[T]] = {
      if (eithers.forall(_.isRight)) {
        val values = eithers.collect { case Right(v) => v }
        Right(values)
      } else {
        val values = eithers.collect { case Left(err) => err.message }
        Left(InvalidParams(msg = values.mkString("\n")))
      }
    }

    def parseTopic(jstr: JString): Either[JsonRpcError, ByteString] = {
      extractBytes(jstr).left.map(_ =>
        InvalidParams(msg = s"Unable to parse topics, expected byte data but got ${jstr.values}")
      )
    }

    def parseNestedTopics(jarr: JArray): Either[JsonRpcError, Seq[ByteString]] = {
      allSuccess(jarr.arr.map {
        case jstr: JString => parseTopic(jstr)
        case other => Left(InvalidParams(msg = s"Unable to parse topics, expected byte data but got: $other"))
      })
    }

    val topicsEither: Either[JsonRpcError, Seq[Seq[ByteString]]] =
      allSuccess((obj \ "topics").extractOpt[JArray].map(_.arr).getOrElse(Nil).map {
        case JNull => Right(Nil)
        case jstr: JString => parseTopic(jstr).map(Seq(_))
        case jarr: JArray => parseNestedTopics(jarr)
        case other => Left(InvalidParams(msg = s"Unable to parse topics, expected byte data or array but got: $other"))
      })

    def optionalBlockParam(field: String) =
      (obj \ field).extractOpt[JValue].flatMap {
        case JNothing => None
        case other => Some(extractBlockParam(other))
      }

    for {
      fromBlock <- toEitherOpt(optionalBlockParam("fromBlock"))
      toBlock <- toEitherOpt(optionalBlockParam("toBlock"))
      address <- toEitherOpt((obj \ "address").extractOpt[String].map(extractAddress))
      topics <- topicsEither
    } yield Filter(fromBlock = fromBlock, toBlock = toBlock, address = address, topics = topics)
  }
}
