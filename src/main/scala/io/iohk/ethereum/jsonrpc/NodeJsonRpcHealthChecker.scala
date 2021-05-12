package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.healthcheck.HealthcheckResponse
import io.iohk.ethereum.jsonrpc.EthBlocksService.{
  BlockByNumberRequest,
  BlockByNumberResponse,
  BestBlockNumberRequest,
  BestBlockNumberResponse
}
import io.iohk.ethereum.jsonrpc.EthInfoService._
import io.iohk.ethereum.jsonrpc.NodeJsonRpcHealthChecker.JsonRpcHealthConfig
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import com.typesafe.config.{Config => TypesafeConfig}
import monix.eval.Task
import java.time.Instant
import java.time.Duration
import akka.actor.ActorRef
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status._
import akka.util.Timeout
import io.iohk.ethereum.utils.AsyncConfig

class NodeJsonRpcHealthChecker(
    netService: NetService,
    ethBlocksService: EthBlocksService,
    syncingController: ActorRef,
    config: JsonRpcHealthConfig,
    asyncConfig: AsyncConfig
) extends JsonRpcHealthChecker {

  implicit val askTimeout: Timeout = asyncConfig.askTimeout

  protected def mainService: String = "node health"

  private var previousBestFetchingBlock: Option[(Instant, BigInt)] = None

  final val peerCountHC = JsonRpcHealthcheck
    .fromServiceResponse("peerCount", netService.peerCount(PeerCountRequest()))
    .map(healthcheck => healthcheck.withInfo(_.value.toString).withPredicate("peer count is 0")(_.value > 0))

  final val storedBlockHC = JsonRpcHealthcheck
    .fromServiceResponse(
      "bestStoredBlock",
      ethBlocksService.getBlockByNumber(BlockByNumberRequest(BlockParam.Latest, fullTxs = true))
    )
    .map(healthcheck =>
      healthcheck
        .collect("No block is currently stored") { case EthBlocksService.BlockByNumberResponse(Some(v)) => v }
        .withInfo(_.number.toString)
    )

  final val bestKnownBlockHC = JsonRpcHealthcheck
    .fromServiceResponse("bestKnownBlock", getBestKnownBlockTask)
    .map(healthcheck => healthcheck.withInfo(_.toString))

  final val fetchingBlockHC = JsonRpcHealthcheck
    .fromServiceResponse("bestFetchingBlock", getBestFetchingBlockTask)
    .map(healthcheck =>
      healthcheck
        .collect("no best fetching block") { case Some(v) => v }
        .withInfo(_.toString)
    )

  final val updateStatusHC = JsonRpcHealthcheck
    .fromServiceResponse("updateStatus", getBestFetchingBlockTask)
    .map(healthcheck =>
      healthcheck
        .collect("no best fetching block") { case Some(v) => v }
        .withPredicate(s"block did not change for more than ${config.noUpdateDurationThreshold.getSeconds()} s")(
          blockNumberHasChanged
        )
    )

  final val syncStatusHC =
    JsonRpcHealthcheck
      .fromTask("syncStatus", syncingController.askFor[SyncProtocol.Status](SyncProtocol.GetStatus))
      .map(healthcheck =>
        healthcheck.withInfo {
          case NotSyncing => "STARTING"
          case s: Syncing if isConsideredSyncing(s.blocksProgress) => "SYNCING"
          case _ => "SYNCED"
        }
      )

  override def healthCheck(): Task[HealthcheckResponse] = {
    val responseTask = Task
      .sequence(
        List(
          peerCountHC,
          storedBlockHC,
          bestKnownBlockHC,
          fetchingBlockHC,
          updateStatusHC,
          syncStatusHC
        )
      )
      .map(_.map(_.toResult))
      .map(HealthcheckResponse)

    handleResponse(responseTask)
  }

  private def blockNumberHasChanged(newBestFetchingBlock: BigInt) =
    previousBestFetchingBlock match {
      case Some((firstSeenAt, value)) if value == newBestFetchingBlock =>
        Instant.now().minus(config.noUpdateDurationThreshold).isBefore(firstSeenAt)
      case _ =>
        previousBestFetchingBlock = Some((Instant.now(), newBestFetchingBlock))
        true
    }

  /** Try to fetch best block number from the sync controller or fallback to ethBlocksService */
  private def getBestKnownBlockTask =
    syncingController
      .askFor[SyncProtocol.Status](SyncProtocol.GetStatus)
      .flatMap {
        case NotSyncing | SyncDone =>
          ethBlocksService
            .bestBlockNumber(EthBlocksService.BestBlockNumberRequest())
            .map(_.map(_.bestBlockNumber))
        case Syncing(_, progress, _) => Task.now(Right(progress.target))
      }

  /** Try to fetch best fetching number from the sync controller or fallback to ethBlocksService */
  private def getBestFetchingBlockTask =
    syncingController
      .askFor[SyncProtocol.Status](SyncProtocol.GetStatus)
      .flatMap {
        case NotSyncing | SyncDone =>
          ethBlocksService
            .getBlockByNumber(BlockByNumberRequest(BlockParam.Pending, fullTxs = true))
            .map(_.map(_.blockResponse.map(_.number)))
        case Syncing(_, progress, _) => Task.now(Right(Some(progress.current)))
      }

  private def isConsideredSyncing(progress: Progress) =
    progress.target - progress.current > config.syncingStatusThreshold

}

object NodeJsonRpcHealthChecker {
  case class JsonRpcHealthConfig(noUpdateDurationThreshold: Duration, syncingStatusThreshold: Int)

  object JsonRpcHealthConfig {
    def apply(rpcConfig: TypesafeConfig): JsonRpcHealthConfig =
      JsonRpcHealthConfig(
        noUpdateDurationThreshold = rpcConfig.getDuration("health.no-update-duration-threshold"),
        syncingStatusThreshold = rpcConfig.getInt("health.syncing-status-threshold")
      )
  }
}
