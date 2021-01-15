package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.healthcheck.HealthcheckResponse
import io.iohk.ethereum.jsonrpc.EthBlocksService.BlockByNumberRequest
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.NetService._
import monix.eval.Task

class NodeJsonRpcHealthChecker(
    netService: NetService,
    ethBlocksService: EthBlocksService
) extends JsonRpcHealthChecker {

  protected def mainService: String = "node health"

  final val listeningHC = JsonRpcHealthcheck("listening", netService.listening(NetService.ListeningRequest()))
  final val peerCountHC = JsonRpcHealthcheck("peerCount", netService.peerCount(PeerCountRequest()))
  final val earliestBlockHC = JsonRpcHealthcheck(
    "earliestBlock",
    ethBlocksService.getBlockByNumber(BlockByNumberRequest(BlockParam.Earliest, fullTxs = true))
  )
  final val latestBlockHC = JsonRpcHealthcheck(
    "latestBlock",
    ethBlocksService.getBlockByNumber(BlockByNumberRequest(BlockParam.Latest, fullTxs = true))
  )
  final val pendingBlockHC = JsonRpcHealthcheck(
    "pendingBlock",
    ethBlocksService.getBlockByNumber(BlockByNumberRequest(BlockParam.Pending, fullTxs = true))
  )

  override def healthCheck(): Task[HealthcheckResponse] = {
    val listeningF = listeningHC()
    val peerCountF = peerCountHC()
    val earliestBlockF = earliestBlockHC()
    val latestBlockF = latestBlockHC()
    val pendingBlockF = pendingBlockHC()

    val allChecksF = List(listeningF, peerCountF, earliestBlockF, latestBlockF, pendingBlockF)
    val responseF = Task.sequence(allChecksF).map(HealthcheckResponse)

    handleResponse(responseF)
  }
}
