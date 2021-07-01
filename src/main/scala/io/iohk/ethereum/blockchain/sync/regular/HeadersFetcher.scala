package io.iohk.ethereum.blockchain.sync.regular
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.{ActorRef => ClassicActorRef}

import monix.eval.Task
import monix.execution.Scheduler

import scala.util.Failure
import scala.util.Success

import org.slf4j.Logger

import io.iohk.ethereum.blockchain.sync.PeersClient.BestPeer
import io.iohk.ethereum.blockchain.sync.PeersClient.Request
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.FetchCommand
import io.iohk.ethereum.blockchain.sync.regular.HeadersFetcher.HeadersFetcherCommand
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockHeaders
import io.iohk.ethereum.network.p2p.messages.ETH62.GetBlockHeaders
import io.iohk.ethereum.utils.Config.SyncConfig

class HeadersFetcher(
    val peersClient: ClassicActorRef,
    val syncConfig: SyncConfig,
    val supervisor: ActorRef[FetchCommand],
    context: ActorContext[HeadersFetcher.HeadersFetcherCommand]
) extends AbstractBehavior[HeadersFetcher.HeadersFetcherCommand](context)
    with FetchRequest[HeadersFetcherCommand] {

  val log: Logger = context.log
  implicit val ec: Scheduler = Scheduler(context.executionContext)

  import HeadersFetcher._

  override def makeAdaptedMessage[T <: Message](peer: Peer, msg: T): HeadersFetcherCommand = AdaptedMessage(peer, msg)

  override def onMessage(message: HeadersFetcherCommand): Behavior[HeadersFetcherCommand] =
    message match {
      case FetchHeaders(blockNumber: BigInt, amount: BigInt) =>
        log.debug("Start fetching headers from block {}", blockNumber)
        requestHeaders(blockNumber, amount)
        Behaviors.same
      case AdaptedMessage(peer, BlockHeaders(headers)) =>
        log.debug("Fetched {} headers starting from block {}", headers.size, headers.headOption.map(_.number))
        supervisor ! BlockFetcher.ReceivedHeaders(peer, headers)
        Behaviors.same
      case HeadersFetcher.RetryHeadersRequest =>
        supervisor ! BlockFetcher.RetryHeadersRequest
        Behaviors.same
      case _ => Behaviors.unhandled
    }

  private def requestHeaders(blockNr: BigInt, amount: BigInt): Unit = {
    log.debug("Fetching headers from block {}", blockNr)
    val msg = GetBlockHeaders(Left(blockNr), amount, skip = 0, reverse = false)

    val resp = makeRequest(Request.create(msg, BestPeer), HeadersFetcher.RetryHeadersRequest)
      .flatMap {
        case AdaptedMessage(_, BlockHeaders(headers)) if headers.isEmpty =>
          log.debug("Empty BlockHeaders response. Retry in {}", syncConfig.syncRetryInterval)
          Task.now(HeadersFetcher.RetryHeadersRequest).delayResult(syncConfig.syncRetryInterval)
        case res => Task.now(res)
      }

    context.pipeToSelf(resp.runToFuture) {
      case Success(res) => res
      case Failure(_)   => HeadersFetcher.RetryHeadersRequest
    }
  }
}

object HeadersFetcher {

  def apply(
      peersClient: ClassicActorRef,
      syncConfig: SyncConfig,
      supervisor: ActorRef[FetchCommand]
  ): Behavior[HeadersFetcherCommand] =
    Behaviors.setup(context => new HeadersFetcher(peersClient, syncConfig, supervisor, context))

  sealed trait HeadersFetcherCommand
  final case class FetchHeaders(blockNumber: BigInt, amount: BigInt) extends HeadersFetcherCommand
  final case object RetryHeadersRequest extends HeadersFetcherCommand
  final private case class AdaptedMessage[T <: Message](peer: Peer, msg: T) extends HeadersFetcherCommand
}
