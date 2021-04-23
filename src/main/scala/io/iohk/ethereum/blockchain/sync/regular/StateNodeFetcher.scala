package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.{ActorRef => ClassicActorRef}
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.sync.PeersClient
import io.iohk.ethereum.blockchain.sync.PeersClient._
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.{FetchCommand, FetchedStateNode}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBodies
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import cats.syntax.either._
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._
import scala.util.{Failure, Success}

class StateNodeFetcher(
                        val peersClient: ClassicActorRef,
                        val syncConfig: SyncConfig,
                        val supervisor: ActorRef[FetchCommand],
                        context: ActorContext[StateNodeFetcher.StateNodeFetcherCommand])
  extends AbstractBehavior[StateNodeFetcher.StateNodeFetcherCommand](context) {

  private val log = context.log
  implicit val ec: Scheduler = Scheduler(context.executionContext)
  implicit val timeout: Timeout = syncConfig.peerResponseTimeout + 2.second // some margin for actor communication

  import StateNodeFetcher._

  private var requester: Option[StateNodeRequester] = None

  override def onMessage(message: StateNodeFetcherCommand): Behavior[StateNodeFetcherCommand] = {
    message match {
      case StateNodeFetcher.FetchStateNode(hash, sender) =>
        log.debug("Start fetching state node")
        requestStateNode(hash)
        requester = Some(StateNodeRequester(hash, sender))
        Behaviors.same
      case AdaptedMessage(peer, NodeData(values)) if requester.isDefined =>
        log.debug("Received state node response from peer {}", peer)

        requester
          .collect(stateNodeRequester => {
            val validatedNode = values
              .asRight[String]
              .ensure(s"Empty response from peer $peer, blacklisting")(_.nonEmpty)
              .ensure("Fetched node state hash doesn't match requested one, blacklisting peer")(nodes =>
                stateNodeRequester.hash == kec256(nodes.head))

            validatedNode match {
              case Left(err) =>
                log.debug(err)
                peersClient ! BlacklistPeer(peer.id, err)
                context.self ! StateNodeFetcher.FetchStateNode(stateNodeRequester.hash, stateNodeRequester.replyTo)
                Behaviors.same[StateNodeFetcherCommand]
              case Right(node) =>
                stateNodeRequester.replyTo ! FetchedStateNode(NodeData(node))
                requester = None
                Behaviors.same[StateNodeFetcherCommand]
            }
          })
          .getOrElse(Behaviors.same)

      case StateNodeFetcher.RetryStateNodeRequest if requester.isDefined =>
        log.debug("Something failed on a state node request, trying again")
        requester
          .collect(stateNodeRequester =>
            context.self ! StateNodeFetcher.FetchStateNode(stateNodeRequester.hash, stateNodeRequester.replyTo))
        Behaviors.same
      case _ => Behaviors.unhandled
    }
  }

  private def requestStateNode(hash: ByteString): Unit = {
    val resp = makeRequest(Request.create(GetNodeData(List(hash)), BestPeer), StateNodeFetcher.RetryStateNodeRequest)
    context.pipeToSelf(resp.runToFuture) {
      case Success(res) => res
      case Failure(_) => StateNodeFetcher.RetryStateNodeRequest
    }
  }

  private def makeRequest(
                           request: Request[_],
                           responseFallback: StateNodeFetcherCommand): Task[StateNodeFetcherCommand] =
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

  private def handleRequestResult(fallback: StateNodeFetcherCommand)(msg: Any): Task[StateNodeFetcherCommand] =
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
        Task.now(StateNodeFetcher.AdaptedMessage(peer, msg))
    }
}

object StateNodeFetcher {

  def apply(
             peersClient: ClassicActorRef,
             syncConfig: SyncConfig,
             supervisor: ActorRef[FetchCommand]): Behavior[StateNodeFetcherCommand] =
    Behaviors.setup(context => new StateNodeFetcher(peersClient, syncConfig, supervisor, context))

  sealed trait StateNodeFetcherCommand
  final case class FetchStateNode(hash: ByteString, originalSender: ClassicActorRef) extends StateNodeFetcherCommand
  final case object RetryStateNodeRequest extends StateNodeFetcherCommand
  private final case class AdaptedMessage[T <: Message](peer: Peer, msg: T) extends StateNodeFetcherCommand

  final case class StateNodeRequester(hash: ByteString, replyTo: ClassicActorRef)
}
