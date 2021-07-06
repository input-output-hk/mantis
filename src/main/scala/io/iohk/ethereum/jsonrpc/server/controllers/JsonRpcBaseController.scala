package io.iohk.ethereum.jsonrpc.server.controllers

import java.time.Duration

import cats.syntax.all._

import monix.eval.Task

import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

import com.typesafe.config.{Config => TypesafeConfig}
import org.json4s.DefaultFormats
import org.json4s.JsonDSL._
import org.json4s.native
import org.json4s.native.Serialization

import io.iohk.ethereum.jsonrpc.JsonRpcControllerMetrics
import io.iohk.ethereum.jsonrpc.JsonRpcError
import io.iohk.ethereum.jsonrpc.JsonRpcError.InternalError
import io.iohk.ethereum.jsonrpc.JsonRpcError.MethodNotFound
import io.iohk.ethereum.jsonrpc.JsonRpcRequest
import io.iohk.ethereum.jsonrpc.JsonRpcResponse
import io.iohk.ethereum.jsonrpc.NodeJsonRpcHealthChecker.JsonRpcHealthConfig
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.jsonrpc.server.ipc.JsonRpcIpcServer.JsonRpcIpcServerConfig
import io.iohk.ethereum.utils.Logger

trait ApisBase {
  def available: List[String]
}

trait JsonRpcBaseController {
  self: ApisBase with Logger =>

  import JsonRpcBaseController._

  /** FIXME: We are making mandatory to pass a config in all the Controllers that implements this trait
    * when it is just used for the disabled methods.
    * We should change this behaviour in order to remove this unnecessary dependency.
    */
  val config: JsonRpcConfig
  implicit def executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global

  def apisHandleFns: Map[String, PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]]]

  def enabledApis: Seq[String]

  implicit val formats: DefaultFormats.type = DefaultFormats

  implicit val serialization: Serialization.type = native.Serialization

  def handleRequest(request: JsonRpcRequest): Task[JsonRpcResponse] = {
    val startTimeNanos = System.nanoTime()

    log.debug(s"received request ${request.inspect}")

    val notFoundFn: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = { case _ =>
      JsonRpcControllerMetrics.NotFoundMethodsCounter.increment()
      Task.now(errorResponse(request, MethodNotFound))
    }

    val handleFn: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] =
      enabledApis.foldLeft(notFoundFn)((fn, api) => apisHandleFns.getOrElse(api, PartialFunction.empty).orElse(fn))

    handleFn(request)
      .flatTap {
        case JsonRpcResponse(_, _, Some(JsonRpcError(code, message, extraData)), _) =>
          Task {
            log.error(
              s"JsonRpcError from request: ${request.toStringWithSensitiveInformation} - response code: $code and message: $message. " +
                s"${extraData.map(data => s"Extra info: ${data.values}")}"
            )
            JsonRpcControllerMetrics.MethodsErrorCounter.increment()
          }
        case JsonRpcResponse(_, _, None, _) =>
          Task {
            JsonRpcControllerMetrics.MethodsSuccessCounter.increment()

            val time = Duration.ofNanos(System.nanoTime() - startTimeNanos)
            JsonRpcControllerMetrics.recordMethodTime(request.method, time)
          }
      }
      .flatTap(response => Task(log.debug(s"sending response ${response.inspect}")))
      .onErrorRecoverWith { case t: Throwable =>
        JsonRpcControllerMetrics.MethodsExceptionCounter.increment()
        log.error(s"Error serving request: ${request.toStringWithSensitiveInformation}", t)
        Task.raiseError(t)
      }
  }

  def handle[Req, Res](
      fn: Req => Task[Either[JsonRpcError, Res]],
      rpcReq: JsonRpcRequest
  )(implicit dec: JsonMethodDecoder[Req], enc: JsonEncoder[Res]): Task[JsonRpcResponse] =
    dec.decodeJson(rpcReq.params) match {
      case Right(req) =>
        fn(req)
          .map {
            case Right(success) => successResponse(rpcReq, success)
            case Left(error)    => errorResponse(rpcReq, error)
          }
          .recover { case ex =>
            log.error("Failed to handle RPC request", ex)
            errorResponse(rpcReq, InternalError)
          }
      case Left(error) =>
        Task.now(errorResponse(rpcReq, error))
    }

  private def successResponse[T](req: JsonRpcRequest, result: T)(implicit enc: JsonEncoder[T]): JsonRpcResponse =
    JsonRpcResponse(req.jsonrpc, Some(enc.encodeJson(result)), None, req.id.getOrElse(0))

  def errorResponse[T](req: JsonRpcRequest, error: JsonRpcError): JsonRpcResponse =
    JsonRpcResponse(req.jsonrpc, None, Some(error), req.id.getOrElse(0))

}

object JsonRpcBaseController {

  trait JsonRpcConfig {
    def apis: Seq[String]
    def accountTransactionsMaxBlocks: Int
    def minerActiveTimeout: FiniteDuration
    def httpServerConfig: JsonRpcHttpServerConfig
    def ipcServerConfig: JsonRpcIpcServerConfig
    def healthConfig: JsonRpcHealthConfig
  }

  object JsonRpcConfig {
    def apply(mantisConfig: TypesafeConfig, availableApis: List[String]): JsonRpcConfig = {
      import scala.concurrent.duration._
      val rpcConfig = mantisConfig.getConfig("network.rpc")

      new JsonRpcConfig {
        override val apis: Seq[String] = {
          val providedApis = rpcConfig.getString("apis").split(",").map(_.trim.toLowerCase)
          val invalidApis = providedApis.diff(availableApis)
          require(
            invalidApis.isEmpty,
            s"Invalid RPC APIs specified: ${invalidApis.mkString(",")}. Availables are ${availableApis.mkString(",")}"
          )
          ArraySeq.unsafeWrapArray(providedApis)
        }

        override def accountTransactionsMaxBlocks: Int = rpcConfig.getInt("account-transactions-max-blocks")
        override def minerActiveTimeout: FiniteDuration = rpcConfig.getDuration("miner-active-timeout").toMillis.millis

        override val httpServerConfig: JsonRpcHttpServerConfig = JsonRpcHttpServerConfig(mantisConfig)
        override val ipcServerConfig: JsonRpcIpcServerConfig = JsonRpcIpcServerConfig(mantisConfig)
        override val healthConfig: JsonRpcHealthConfig = JsonRpcHealthConfig(rpcConfig)
      }
    }
  }
}
