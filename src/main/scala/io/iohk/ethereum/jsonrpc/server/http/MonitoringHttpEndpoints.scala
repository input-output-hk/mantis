package io.iohk.ethereum.jsonrpc.server.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import io.iohk.ethereum.jsonrpc.JsonRpcHealthChecker
import io.iohk.ethereum.utils.BuildInfo
import monix.execution.Scheduler
import org.json4s.{DefaultFormats, Formats, Serialization}
import org.json4s.native.Serialization

trait MonitoringHttpEndpoints {
  val jsonRpcHealthChecker: JsonRpcHealthChecker
  implicit def serialization: Serialization
  implicit def formats: Formats
  implicit def scheduler: Scheduler

  protected def handleHealthcheck(): StandardRoute = {
    val responseF = jsonRpcHealthChecker.healthCheck()
    val httpResponseF =
      responseF.map {
        case response if response.isOK =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(ContentTypes.`application/json`, serialization.writePretty(response))
          )
        case response =>
          HttpResponse(
            status = StatusCodes.InternalServerError,
            entity = HttpEntity(ContentTypes.`application/json`, serialization.writePretty(response))
          )
      }
    complete(httpResponseF.runToFuture(scheduler))
  }

  protected def handleBuildInfo(): StandardRoute = {
    val buildInfo = Serialization.writePretty(BuildInfo.toMap)(DefaultFormats)
    complete(
      HttpResponse(
        status = StatusCodes.OK,
        entity = HttpEntity(ContentTypes.`application/json`, buildInfo)
      )
    )
  }

}
