package io.iohk.ethereum.faucet.jsonrpc

import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain.StatusRequest
import io.iohk.ethereum.healthcheck.HealthcheckResponse
import io.iohk.ethereum.jsonrpc.{JsonRpcHealthChecker, JsonRpcHealthcheck}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FaucetJsonRpcHealthCheck(faucetRpcService: FaucetRpcService) extends JsonRpcHealthChecker {

  protected def mainService: String = "faucet health"

  final val statusHC = JsonRpcHealthcheck("status", () => faucetRpcService.status(StatusRequest()))

  override def healthCheck(): Future[HealthcheckResponse] = {
    val statusF = statusHC()
    val responseF = statusF.map(check => HealthcheckResponse(List(check)))

    handleResponse(responseF)
  }
}
