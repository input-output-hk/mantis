package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorRef
import io.iohk.ethereum.blockchain.sync.PeersClient
import io.iohk.ethereum.blockchain.sync.PeersClient.{BlacklistPeer, NoSuitablePeer, Request, RequestFailed}
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.eval.Task
import org.slf4j.Logger
import akka.pattern.ask
import akka.util.Timeout
import io.iohk.ethereum.utils.FunctorOps._

import scala.concurrent.duration._
import scala.util.Failure

trait FetchRequest[A] {
  val peersClient: ActorRef
  val syncConfig: SyncConfig
  val log: Logger

  def makeAdaptedMessage[T <: Message](peer: Peer, msg: T): A

  implicit val timeout: Timeout = syncConfig.peerResponseTimeout + 2.second // some margin for actor communication

  def makeRequest(request: Request[_], responseFallback: A): Task[A] =
    Task
      .deferFuture(peersClient ? request)
      .tap(blacklistPeerOnFailedRequest)
      .flatMap(handleRequestResult(responseFallback))
      .onErrorHandle { error =>
        log.error("Unexpected error while doing a request", error)
        responseFallback
      }

  def blacklistPeerOnFailedRequest(msg: Any): Unit = msg match {
    case RequestFailed(peer, reason) => peersClient ! BlacklistPeer(peer.id, reason)
    case _ => ()
  }

  def handleRequestResult(fallback: A)(msg: Any): Task[A] = {
    msg match {
      case failed: RequestFailed =>
        log.debug("Request failed due to {}", failed)
        Task.now(fallback)
      case NoSuitablePeer =>
        Task.now(fallback).delayExecution(syncConfig.syncRetryInterval)
      case Failure(cause) =>
        log.error("Unexpected error on the request result", cause)
        Task.now(fallback)
      case PeersClient.Response(peer, msg) =>
        Task.now(makeAdaptedMessage(peer, msg))
    }
  }
}
