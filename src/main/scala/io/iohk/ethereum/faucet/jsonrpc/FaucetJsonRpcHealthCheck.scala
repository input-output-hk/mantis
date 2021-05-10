package io.iohk.ethereum.faucet.jsonrpc

import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain.StatusRequest
import io.iohk.ethereum.healthcheck.HealthcheckResponse
import io.iohk.ethereum.jsonrpc.{JsonRpcHealthChecker, JsonRpcHealthcheck}
import monix.eval.Task

class FaucetJsonRpcHealthCheck(faucetRpcService: FaucetRpcService) extends JsonRpcHealthChecker {

  protected def mainService: String = "faucet health"

  final val statusHC = JsonRpcHealthcheck.fromServiceResponse("status", faucetRpcService.status(StatusRequest()))

  override def healthCheck(): Task[HealthcheckResponse] = {
    val statusF = statusHC.toTask
    val responseF = statusF.map(check => HealthcheckResponse(List(check)))

    handleResponse(responseF)
  }
}
