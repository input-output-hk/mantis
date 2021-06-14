package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.{ActorRef => ClassicActorRef}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.PeersClient.{BestPeer, Request}
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.FetchCommand
import io.iohk.ethereum.blockchain.sync.regular.BodiesFetcher.BodiesFetcherCommand
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.ETH62.{BlockBodies, GetBlockBodies}
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.execution.Scheduler

import scala.util.{Failure, Success}

class BodiesFetcher(
    val peersClient: ClassicActorRef,
    val syncConfig: SyncConfig,
    val supervisor: ActorRef[FetchCommand],
    context: ActorContext[BodiesFetcher.BodiesFetcherCommand]
) extends AbstractBehavior[BodiesFetcher.BodiesFetcherCommand](context)
    with FetchRequest[BodiesFetcherCommand] {

  val log = context.log
  implicit val ec: Scheduler = Scheduler(context.executionContext)

  import BodiesFetcher._

  override def makeAdaptedMessage[T <: Message](peer: Peer, msg: T): BodiesFetcherCommand = AdaptedMessage(peer, msg)

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
}

object BodiesFetcher {

  def apply(
      peersClient: ClassicActorRef,
      syncConfig: SyncConfig,
      supervisor: ActorRef[FetchCommand]
  ): Behavior[BodiesFetcherCommand] =
    Behaviors.setup(context => new BodiesFetcher(peersClient, syncConfig, supervisor, context))

  sealed trait BodiesFetcherCommand
  final case class FetchBodies(hashes: Seq[ByteString]) extends BodiesFetcherCommand
  final case object RetryBodiesRequest extends BodiesFetcherCommand
  private final case class AdaptedMessage[T <: Message](peer: Peer, msg: T) extends BodiesFetcherCommand
}
