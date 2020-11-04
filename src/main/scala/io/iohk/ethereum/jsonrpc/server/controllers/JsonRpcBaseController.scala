package io.iohk.ethereum.jsonrpc.server.controllers

import java.util.concurrent.TimeUnit

import com.typesafe.config.{Config => TypesafeConfig}
import io.iohk.ethereum.jsonrpc.JsonRpcError.{InternalError, MethodNotFound}
import io.iohk.ethereum.jsonrpc.{JsonRpcControllerMetrics, JsonRpcError, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodDecoder}
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.jsonrpc.server.ipc.JsonRpcIpcServer.JsonRpcIpcServerConfig
import io.iohk.ethereum.utils.Logger
import org.json4s.{DefaultFormats, native}
import org.json4s.JsonDSL._
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

trait ApisBase {
  def available: List[String]
}

trait JsonRpcBaseController {
  self: ApisBase with Logger =>

  import JsonRpcBaseController._

  /**
    * FIXME: We are making mandatory to pass a config in all the Controllers that implements this trait
    * when it is just used for the disabled methods.
    * We should change this behaviour in order to remove this unnecessary dependency.
    */
  val config: JsonRpcConfig
  implicit def executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global

  def apisHandleFns: Map[String, PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]]]

  def enabledApis: Seq[String]

  implicit val formats = DefaultFormats

  implicit val serialization = native.Serialization

  def handleRequest(request: JsonRpcRequest): Future[JsonRpcResponse] = {
    val startTimeNanos = System.nanoTime()

    val notFoundFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = { case _ =>
      JsonRpcControllerMetrics.NotFoundMethodsCounter.increment()
      Future.successful(errorResponse(request, MethodNotFound))
    }

    val handleFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] =
      enabledApis.foldLeft(notFoundFn)((fn, api) => apisHandleFns.getOrElse(api, PartialFunction.empty) orElse fn)

    handleFn(request).andThen {
      case Success(JsonRpcResponse(_, _, Some(JsonRpcError(_, _, _)), _)) =>
        JsonRpcControllerMetrics.MethodsErrorCounter.increment()

      case Success(JsonRpcResponse(_, _, None, _)) =>
        JsonRpcControllerMetrics.MethodsSuccessCounter.increment()

        val endTimeNanos = System.nanoTime()
        val dtNanos = endTimeNanos - startTimeNanos
        JsonRpcControllerMetrics.MethodsTimer.record(dtNanos, TimeUnit.NANOSECONDS)

      case Failure(t) =>
        JsonRpcControllerMetrics.MethodsExceptionCounter.increment()
        log.error("Error serving request", t)
    }
  }

  def handle[Req, Res](
      fn: Req => Future[Either[JsonRpcError, Res]],
      rpcReq: JsonRpcRequest
  )(implicit dec: JsonMethodDecoder[Req], enc: JsonEncoder[Res]): Future[JsonRpcResponse] = {
    dec.decodeJson(rpcReq.params) match {
      case Right(req) =>
        fn(req)
          .map {
            case Right(success) => successResponse(rpcReq, success)
            case Left(error) => errorResponse(rpcReq, error)
          }
          .recover { case ex =>
            log.error("Failed to handle RPC request", ex)
            errorResponse(rpcReq, InternalError)
          }
      case Left(error) =>
        Future.successful(errorResponse(rpcReq, error))
    }
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
          providedApis
        }

        override def accountTransactionsMaxBlocks: Int = rpcConfig.getInt("account-transactions-max-blocks")
        override def minerActiveTimeout: FiniteDuration = rpcConfig.getDuration("miner-active-timeout").toMillis.millis

        override val httpServerConfig: JsonRpcHttpServerConfig = JsonRpcHttpServerConfig(mantisConfig)
        override val ipcServerConfig: JsonRpcIpcServerConfig = JsonRpcIpcServerConfig(mantisConfig)
      }
    }
  }
}
