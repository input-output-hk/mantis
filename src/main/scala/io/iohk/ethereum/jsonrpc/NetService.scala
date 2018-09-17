package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.Timeout
import io.iohk.ethereum.jsonrpc.NetService.NetServiceConfig
import io.iohk.ethereum.network.PeerManagerActor
import io.iohk.ethereum.utils.ServerStatus.{Listening, NotListening}
import io.iohk.ethereum.utils.{Config, NodeStatus}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

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
      NetServiceConfig(
        peerManagerTimeout = netServiceConfig.getDuration("peer-manager-timeout").toMillis.millis)
    }
  }
}

class NetService(nodeStatusHolder: AtomicReference[NodeStatus], peerManager: ActorRef, config: NetServiceConfig) {
  import NetService._

  def version(req: VersionRequest): ServiceResponse[VersionResponse] =
    Future.successful(Right(VersionResponse(Config.Network.peer.networkId.toString)))

  def listening(req: ListeningRequest): ServiceResponse[ListeningResponse] = {
    Future.successful {
      Right(
        nodeStatusHolder.get().serverStatus match {
          case _: Listening => ListeningResponse(true)
          case NotListening => ListeningResponse(false)
        }
      )
    }
  }

  def peerCount(req: PeerCountRequest): ServiceResponse[PeerCountResponse] = {
    import akka.pattern.ask
    implicit val timeout = Timeout(config.peerManagerTimeout)

    (peerManager ? PeerManagerActor.GetPeers)
      .mapTo[PeerManagerActor.Peers]
      .map { peers => Right(PeerCountResponse(peers.handshaked.size)) }
  }

}
