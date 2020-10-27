package io.iohk.ethereum.forking

import java.io.{PrintWriter, StringWriter}
import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.http.scaladsl.{ConnectionContext, Http}
import cats.data.EitherT
import io.circe.generic.semiauto.deriveCodec
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Json}
import io.iohk.ethereum.utils.Logger
import javax.net.ssl.SSLContext
import monix.eval.Task
import mouse.all.anySyntaxMouse

import scala.language.implicitConversions

abstract class RpcClient(node: Uri, maybeSslContext: Option[SSLContext])(implicit system: ActorSystem) extends Logger {
  import RpcClient._

  lazy val connectionContext = maybeSslContext.fold(Http().defaultClientHttpsContext)(ConnectionContext.httpsClient)

  protected def doRequest[T: Decoder](
      method: String,
      params: Seq[Json] = Seq.empty
  ): RpcResponse[T] = {
    doJsonRequest(method, params).subflatMap(getResult[T])
  }

  protected def doJsonRequest(method: String, params: Seq[Json]): RpcResponse[Json] = {
    val request = prepareJsonRequest(method, params)
    log.debug(s"Making RPC call with request: $request")
    makeRpcCall(request.asJson)
  }

  private def getResult[T: Decoder](jsonResponse: Json): Either[RpcClientError, T] = {
    log.debug(s"Got response: $jsonResponse")
    jsonResponse.hcursor.downField("error").as[JsonRpcError] match {
      case Right(error: JsonRpcError) =>
        Left(RpcClientError(s"Node returned an error: ${error.message} (${error.code})"))
      case Left(_) =>
        jsonResponse.hcursor.downField("result").as[T].left.map(f => RpcClientError(f.message))
    }
  }

  private def makeRpcCall(jsonRequest: Json): RpcResponse[Json] = {
    val entity = HttpEntity(ContentTypes.`application/json`, jsonRequest.noSpaces)
    val request = HttpRequest(method = HttpMethods.POST, uri = node, entity = entity)

    EitherT(
      (for {
        resp <- Task.deferFuture(Http().singleRequest(request, connectionContext))
        data <- Task.deferFuture(Unmarshal(resp.entity).to[String])
      } yield parse(data).left.map(e => RpcClientError(e.message)))
        .onErrorRecover { case ex =>
          Left(RpcClientError("RPC request failed: " + exceptionToString(ex)))
        }
    )
  }

  private def prepareJsonRequest(method: String, params: Seq[Json]): Map[String, Json] = {
    Map(
      "jsonrpc" -> "2.0".asJson,
      "method" -> method.asJson,
      "id" -> UUID.randomUUID().asJson
    ) |> (map => if (params.isEmpty) map else map + ("params" -> params.asJson))
  }

  private def exceptionToString(ex: Throwable): String = {
    val sw = new StringWriter()
    sw.append(ex.getMessage + "\n")
    ex.printStackTrace(new PrintWriter(sw))
    sw.toString
  }
}

object RpcClient {
  type RpcResponse[T] = EitherT[Task, RpcClientError, T]
  object RpcResponse {
    def unit: RpcResponse[Unit] = EitherT.pure[Task, RpcClientError](())
  }

  implicit class RpcResponseOps[T](res: RpcResponse[T]) {
    def peel: Task[T] = res.foldF(err => Task.raiseError(err.toThrowable), Task.pure)
  }

  type Secrets = Map[String, Json]

  final case class RpcClientError(message: String) extends AnyVal {
    def toThrowable: Throwable = {
      println(message)
      new Exception(message)
    }
  }

  case class JsonRpcError(code: Int, message: String)
  object JsonRpcError {
    implicit val jsonRpcErrorCodec: Codec[JsonRpcError] = deriveCodec
  }
}
