package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.healthcheck.HealthcheckResponse

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

trait JsonRpcHealthChecker {
  def healthCheck(): Future[HealthcheckResponse]

  def handleResponse(responseF: Future[HealthcheckResponse]): Future[HealthcheckResponse] = {
    responseF.andThen {
      case Success(response) if (!response.isOK) =>
        JsonRpcControllerMetrics.HealhcheckErrorCounter.increment()
      case Failure(t) =>
        JsonRpcControllerMetrics.HealhcheckErrorCounter.increment()
    }
  }

}
