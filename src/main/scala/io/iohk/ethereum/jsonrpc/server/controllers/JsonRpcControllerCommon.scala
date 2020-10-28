package io.iohk.ethereum.jsonrpc.server.controllers

import java.util.concurrent.TimeUnit

import com.typesafe.config.{Config => TypesafeConfig}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors._
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.jsonrpc.server.ipc.JsonRpcIpcServer.JsonRpcIpcServerConfig
import io.iohk.ethereum.jsonrpc.{JsonRpcControllerMetrics, JsonRpcError, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.utils.Logger
import mouse.all._
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.JsonDSL._
import org.json4s.{DefaultFormats, native}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait JsonRpcControllerCommon extends Logger {

  import JsonRpcControllerCommon._

  private val self = this

  /**
    * FIXME: We are making mandatory to pass a config in all the Controllers that implements this trait
    * when it is just used for the disabled methods.
    * We should change this behaviour in order to remove this unnecessary dependency.
    * */
  val config: JsonRpcConfig
  implicit def executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global

  def handleFn: Map[String, PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]]]

  def enabledApis: Seq[String]

  implicit val formats = DefaultFormats

  implicit val serialization = native.Serialization


  def handle[Req, Res](
                                fn: Req => Future[Either[JsonRpcError, Res]],
                                rpcReq: JsonRpcRequest
                              )(implicit dec: JsonDecoder[Req], enc: JsonEncoder[Res]): Future[JsonRpcResponse] = {
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

  def handleRequest(request: JsonRpcRequest): Future[JsonRpcResponse] = {
    val startTimeNanos = System.nanoTime()

    val notFoundFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = { case _ =>
      JsonRpcControllerMetrics.NotFoundMethodsCounter.increment()
      Future.successful(errorResponse(request, MethodNotFound))
    }

    val handleFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] =
      enabledApis.foldLeft(notFoundFn)((fn, api) => this.handleFn.getOrElse(api, PartialFunction.empty) orElse fn)

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

  def successResponse[T](req: JsonRpcRequest, result: T)(implicit enc: JsonEncoder[T]): JsonRpcResponse =
    JsonRpcResponse(req.jsonrpc, Some(enc.encodeJson(result)), None, req.id.getOrElse(0))

  def errorResponse[T](req: JsonRpcRequest, error: JsonRpcError): JsonRpcResponse =
    JsonRpcResponse(req.jsonrpc, None, Some(error), req.id.getOrElse(0))

  def shutdown(): Unit = {}

}

object JsonRpcControllerCommon {

  /**
    * Json Decoder used for RPC methods
    */
  trait JsonDecoder[T] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, T]
  }

  object JsonDecoder {
    class NoParamsDecoder[T](request: => T) extends JsonDecoder[T] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, T] =
        params match {
          case None | Some(JArray(Nil)) => Right(request)
          case _ => Left(InvalidParams(s"No parameters expected"))
        }
    }
  }

  /**
    * Json Decoder used for single values
    */
  trait ValueJsonDecoder[T] {
    def decodeJson(value: JValue): Either[JsonRpcError, T]
  }
  object ValueJsonDecoder {
    def apply[T](implicit decoder: ValueJsonDecoder[T]): ValueJsonDecoder[T] = decoder
  }

  trait JsonEncoder[T] { self =>
    def encodeJson(t: T): JValue

    def contramap[S](cb: S => T): JsonEncoder[S] = (s: S) => s |> cb |> self.encodeJson
  }
  object JsonEncoder {
    def apply[T](implicit encoder: JsonEncoder[T]): JsonEncoder[T] = encoder
  }

  trait Codec[Req, Res] extends JsonDecoder[Req] with JsonEncoder[Res]
  object Codec {
    import scala.language.implicitConversions

    implicit def decoderWithEncoderIntoCodec[Req, Res](
                                                        decEnc: JsonDecoder[Req] with JsonEncoder[Res]
                                                      ): Codec[Req, Res] = new Codec[Req, Res] {
      def decodeJson(params: Option[JArray]) = decEnc.decodeJson(params)
      def encodeJson(t: Res) = decEnc.encodeJson(t)
    }
  }

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
          require(invalidApis.isEmpty,
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

