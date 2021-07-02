package io.iohk.ethereum.faucet.jsonrpc

import monix.eval.Task

import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain.StatusRequest
import io.iohk.ethereum.healthcheck.HealthcheckResponse
import io.iohk.ethereum.jsonrpc.JsonRpcHealthChecker
import io.iohk.ethereum.jsonrpc.JsonRpcHealthcheck

class FaucetJsonRpcHealthCheck(faucetRpcService: FaucetRpcService) extends JsonRpcHealthChecker {

  protected def mainService: String = "faucet health"

  final val statusHC: Task[JsonRpcHealthcheck[FaucetDomain.StatusResponse]] =
    JsonRpcHealthcheck.fromServiceResponse("status", faucetRpcService.status(StatusRequest()))

  override def healthCheck(): Task[HealthcheckResponse] = {
    val statusF = statusHC.map(_.toResult)
    val responseF = statusF.map(check => HealthcheckResponse(List(check)))

    handleResponse(responseF)
  }
}
