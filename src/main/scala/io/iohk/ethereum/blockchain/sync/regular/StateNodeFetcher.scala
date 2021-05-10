package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.{ActorRef => ClassicActorRef}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.PeersClient._
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.{FetchCommand, FetchedStateNode}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.utils.Config.SyncConfig
import cats.syntax.either._
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason
import monix.execution.Scheduler

import scala.util.{Failure, Success}

class StateNodeFetcher(
    val peersClient: ClassicActorRef,
    val syncConfig: SyncConfig,
    val supervisor: ActorRef[FetchCommand],
    context: ActorContext[StateNodeFetcher.StateNodeFetcherCommand]
) extends AbstractBehavior[StateNodeFetcher.StateNodeFetcherCommand](context)
    with FetchRequest[StateNodeFetcher.StateNodeFetcherCommand] {

  val log = context.log
  implicit val ec: Scheduler = Scheduler(context.executionContext)

  import StateNodeFetcher._

  override def makeAdaptedMessage[T <: Message](peer: Peer, msg: T): StateNodeFetcherCommand = AdaptedMessage(peer, msg)

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
              .asRight[BlacklistReason]
              .ensure(BlacklistReason.EmptyStateNodeResponse)(_.nonEmpty)
              .ensure(BlacklistReason.WrongStateNodeResponse)(nodes => stateNodeRequester.hash == kec256(nodes.head))

            validatedNode match {
              case Left(err) =>
                log.debug(err.description)
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
            context.self ! StateNodeFetcher.FetchStateNode(stateNodeRequester.hash, stateNodeRequester.replyTo)
          )
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
}

object StateNodeFetcher {

  def apply(
      peersClient: ClassicActorRef,
      syncConfig: SyncConfig,
      supervisor: ActorRef[FetchCommand]
  ): Behavior[StateNodeFetcherCommand] =
    Behaviors.setup(context => new StateNodeFetcher(peersClient, syncConfig, supervisor, context))

  sealed trait StateNodeFetcherCommand
  final case class FetchStateNode(hash: ByteString, originalSender: ClassicActorRef) extends StateNodeFetcherCommand
  final case object RetryStateNodeRequest extends StateNodeFetcherCommand
  private final case class AdaptedMessage[T <: Message](peer: Peer, msg: T) extends StateNodeFetcherCommand

  final case class StateNodeRequester(hash: ByteString, replyTo: ClassicActorRef)
}
