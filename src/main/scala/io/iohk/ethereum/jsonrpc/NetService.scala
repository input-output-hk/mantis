package io.iohk.ethereum.jsonrpc

import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import akka.util.Timeout

import monix.eval.Task

import scala.concurrent.duration._

import io.iohk.ethereum.jsonrpc.NetService.NetServiceConfig
import io.iohk.ethereum.network.PeerManagerActor
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.NodeStatus
import io.iohk.ethereum.utils.ServerStatus.Listening
import io.iohk.ethereum.utils.ServerStatus.NotListening

object NetService {
  case class VersionRequest()
  case class VersionResponse(value: String)

  case class ListeningRequest()
  case class ListeningResponse(value: Boolean)

  case class PeerCountRequest()
  case class PeerCountResponse(value: Int)

  case class NetServiceConfig(peerManagerTimeout: FiniteDuration)

  object NetServiceConfig {
    def apply(etcClientConfig: com.typesafe.config.Config): NetServiceConfig = {
      val netServiceConfig = etcClientConfig.getConfig("network.rpc.net")
      NetServiceConfig(peerManagerTimeout = netServiceConfig.getDuration("peer-manager-timeout").toMillis.millis)
    }
  }
}

class NetService(nodeStatusHolder: AtomicReference[NodeStatus], peerManager: ActorRef, config: NetServiceConfig) {
  import NetService._

  def version(req: VersionRequest): ServiceResponse[VersionResponse] =
    Task.now(Right(VersionResponse(Config.Network.peer.networkId.toString)))

  def listening(req: ListeningRequest): ServiceResponse[ListeningResponse] =
    Task.now {
      Right(
        nodeStatusHolder.get().serverStatus match {
          case _: Listening => ListeningResponse(true)
          case NotListening => ListeningResponse(false)
        }
      )
    }

  def peerCount(req: PeerCountRequest): ServiceResponse[PeerCountResponse] = {
    implicit val timeout: Timeout = Timeout(config.peerManagerTimeout)
    import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
    peerManager
      .askFor[PeerManagerActor.Peers](PeerManagerActor.GetPeers)
      .map(peers => Right(PeerCountResponse(peers.handshaked.size)))
  }
}
