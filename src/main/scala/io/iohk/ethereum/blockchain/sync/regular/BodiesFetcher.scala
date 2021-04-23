package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.typed.{ActorRef, Behavior}
import akka.pattern.ask
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.{ActorRef => ClassicActorRef}
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.sync.PeersClient
import io.iohk.ethereum.blockchain.sync.PeersClient.{BestPeer, BlacklistPeer, NoSuitablePeer, Request, RequestFailed}
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.FetchCommand
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, GetBlockBodies}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._
import scala.util.{Failure, Success}

class BodiesFetcher(
                     val peersClient: ClassicActorRef,
                     val syncConfig: SyncConfig,
                     val supervisor: ActorRef[FetchCommand],
                     context: ActorContext[BodiesFetcher.BodiesFetcherCommand])
  extends AbstractBehavior[BodiesFetcher.BodiesFetcherCommand](context) {

  private val log = context.log
  implicit val ec: Scheduler = Scheduler(context.executionContext)
  implicit val timeout: Timeout = syncConfig.peerResponseTimeout + 2.second // some margin for actor communication

  import BodiesFetcher._

  override def onMessage(message: BodiesFetcherCommand): Behavior[BodiesFetcherCommand] = {
    message match {
      case FetchBodies(hashes) =>
        log.debug("Start fetching bodies")
        requestBodies(hashes)
        Behaviors.same
      case AdaptedMessage(peer, BlockBodies(bodies)) =>
        log.debug(s"Received ${bodies.size} block bodies")
        supervisor ! BlockFetcher.ReceivedBodies(peer, bodies)
        Behaviors.same
      case BodiesFetcher.RetryBodiesRequest =>
        supervisor ! BlockFetcher.RetryBodiesRequest
        Behaviors.same
      case _ => Behaviors.unhandled
    }
  }

  private def requestBodies(hashes: Seq[ByteString]): Unit = {
    val resp = makeRequest(Request.create(GetBlockBodies(hashes), BestPeer), BodiesFetcher.RetryBodiesRequest)
    context.pipeToSelf(resp.runToFuture) {
      case Success(res) => res
      case Failure(_) => BodiesFetcher.RetryBodiesRequest
    }
  }

  private def makeRequest(request: Request[_], responseFallback: BodiesFetcherCommand): Task[BodiesFetcherCommand] =
    Task
      .deferFuture(peersClient ? request)
      .tap(blacklistPeerOnFailedRequest)
      .flatMap(handleRequestResult(responseFallback))
      .onErrorHandle { error =>
        log.error("Unexpected error while doing a request", error)
        responseFallback
      }

  private def blacklistPeerOnFailedRequest(msg: Any): Unit = msg match {
    case RequestFailed(peer, reason) => peersClient ! BlacklistPeer(peer.id, reason)
    case _ => ()
  }

  private def handleRequestResult(fallback: BodiesFetcherCommand)(msg: Any): Task[BodiesFetcherCommand] = msg match {
    case failed: RequestFailed =>
      log.debug("Request failed due to {}", failed)
      Task.now(fallback)
    case NoSuitablePeer =>
      Task.now(fallback).delayExecution(syncConfig.syncRetryInterval)
    case Failure(cause) =>
      log.error("Unexpected error on the request result", cause)
      Task.now(fallback)
    case PeersClient.Response(peer, msg) =>
      Task.now(BodiesFetcher.AdaptedMessage(peer, msg))
  }
}

object BodiesFetcher {

  def apply(
             peersClient: ClassicActorRef,
             syncConfig: SyncConfig,
             supervisor: ActorRef[FetchCommand]): Behavior[BodiesFetcherCommand] =
    Behaviors.setup(context => new BodiesFetcher(peersClient, syncConfig, supervisor, context))

  sealed trait BodiesFetcherCommand
  final case class FetchBodies(hashes: Seq[ByteString]) extends BodiesFetcherCommand
  final case object RetryBodiesRequest extends BodiesFetcherCommand
  private final case class AdaptedMessage[T <: Message](peer: Peer, msg: T) extends BodiesFetcherCommand
}
