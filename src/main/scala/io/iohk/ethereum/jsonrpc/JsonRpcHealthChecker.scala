package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.healthcheck.HealthcheckResponse
import monix.eval.Task

trait JsonRpcHealthChecker {
  def healthCheck(): Task[HealthcheckResponse]

  def handleResponse(responseF: Task[HealthcheckResponse]): Task[HealthcheckResponse] =
    responseF
      .map {
        case response if !response.isOK =>
          JsonRpcControllerMetrics.HealhcheckErrorCounter.increment()
          response
        case response => response
      }
      .onErrorHandleWith { t =>
        JsonRpcControllerMetrics.HealhcheckErrorCounter.increment()
        Task.raiseError(t)
      }
}
