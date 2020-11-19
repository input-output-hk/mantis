package io.iohk.ethereum.jsonrpc.client

import java.io.{PrintWriter, StringWriter}
import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import io.circe.generic.auto._
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, Json}
import io.iohk.ethereum.jsonrpc.JsonRpcError
import io.iohk.ethereum.utils.Logger
import javax.net.ssl.SSLContext
import monix.eval.Task

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

abstract class RpcBaseClient(node: Uri, maybeSslContext: Option[SSLContext])(implicit
    system: ActorSystem,
    ec: ExecutionContext
) extends Logger {

  import RpcBaseClient._

  //TODO....
  lazy val connectionContext: HttpsConnectionContext =
    maybeSslContext.fold(Http().defaultClientHttpsContext)(ConnectionContext.httpsClient)

  def shutdown(): Unit = {
    Await.ready(system.terminate(), 5.seconds)
  }

  protected def doRequest[T: Decoder](method: String, args: Seq[Json]): RpcResponse[T] = {
    doJsonRequest(method, args).map(_.flatMap(getResult[T]))
  }

  protected def doJsonRequest(
      method: String,
      args: Seq[Json]
  ): RpcResponse[Json] = {
    val request = prepareJsonRequest(method, args)
    log.info(s"Making RPC call with request: $request")
    makeRpcCall(request.asJson)
  }

  private def getResult[T: Decoder](jsonResponse: Json): Either[RpcError, T] = {
    jsonResponse.hcursor.downField("error").as[JsonRpcError] match {
      case Right(error) =>
        Left(RpcClientError(s"Node returned an error: ${error.message} (${error.code})"))
      case Left(_) =>
        jsonResponse.hcursor.downField("result").as[T].left.map(f => RpcClientError(f.message))
    }
  }

  private def makeRpcCall(jsonRequest: Json): Task[Either[RpcError, Json]] = {
    val entity = HttpEntity(ContentTypes.`application/json`, jsonRequest.noSpaces)
    val request = HttpRequest(method = HttpMethods.POST, uri = node, entity = entity)

    Task
      .deferFuture(for {
        resp <- Http().singleRequest(request, connectionContext)
        data <- Unmarshal(resp.entity).to[String]
      } yield parse(data).left.map(e => RpcClientError(e.message)))
      .onErrorHandle { ex: Throwable =>
        Left(RpcClientError(s"RPC request failed: ${exceptionToString(ex)}"))
      }
  }

  private def prepareJsonRequest(method: String, args: Seq[Json]): Json = {
    Map(
      "jsonrpc" -> "2.0".asJson,
      "method" -> method.asJson,
      "params" -> args.asJson,
      "id" -> s"${UUID.randomUUID()}".asJson
    ).asJson
  }

  private def exceptionToString(ex: Throwable): String = {
    val sw = new StringWriter()
    sw.append(ex.getMessage + "\n")
    ex.printStackTrace(new PrintWriter(sw))
    sw.toString
  }

}

object RpcBaseClient {
  type RpcResponse[T] = Task[Either[RpcError, T]]

  type Secrets = Map[String, Json]

  sealed trait RpcError {
    def msg: String
  }

  case class ParserError(msg: String) extends RpcError

  case class RpcClientError(msg: String) extends RpcError
}
