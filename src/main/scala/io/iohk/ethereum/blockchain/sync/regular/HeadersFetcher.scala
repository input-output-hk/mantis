package io.iohk.ethereum.blockchain.sync.regular
import akka.actor.typed.{ActorRef, Behavior}
import akka.pattern.ask
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.{ActorRef => ClassicActorRef}
import akka.util.Timeout
import io.iohk.ethereum.blockchain.sync.PeersClient
import io.iohk.ethereum.blockchain.sync.PeersClient.{BestPeer, BlacklistPeer, NoSuitablePeer, Request, RequestFailed}
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.FetchCommand
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._
import scala.util.{Failure, Success}

class HeadersFetcher(
                      val peersClient: ClassicActorRef,
                      val syncConfig: SyncConfig,
                      val supervisor: ActorRef[FetchCommand],
                      context: ActorContext[HeadersFetcher.HeadersFetcherCommand])
  extends AbstractBehavior[HeadersFetcher.HeadersFetcherCommand](context) {

  private val log = context.log
  implicit val ec: Scheduler = Scheduler(context.executionContext)
  implicit val timeout: Timeout = syncConfig.peerResponseTimeout + 2.second // some margin for actor communication

  import HeadersFetcher._

  override def onMessage(message: HeadersFetcherCommand): Behavior[HeadersFetcherCommand] = {
    message match {
      case FetchHeaders(blockNumber: BigInt, amount: BigInt) =>
        log.debug("Start fetching headers from block {}", blockNumber)
        requestHeaders(blockNumber, amount)
        Behaviors.same
      case AdaptedMessage(_, BlockHeaders(headers)) =>
        log.debug("Fetched {} headers starting from block {}", headers.size, headers.headOption.map(_.number))
        supervisor ! BlockFetcher.ReceivedHeaders(headers)
        Behaviors.same
      case HeadersFetcher.RetryHeadersRequest =>
        supervisor ! BlockFetcher.RetryHeadersRequest
        Behaviors.same
      case _ => Behaviors.unhandled
    }
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
      case Failure(_) => HeadersFetcher.RetryHeadersRequest
    }
  }

  private def makeRequest(request: Request[_], responseFallback: HeadersFetcherCommand): Task[HeadersFetcherCommand] =
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

  private def handleRequestResult(fallback: HeadersFetcherCommand)(msg: Any): Task[HeadersFetcherCommand] = msg match {
    case failed: RequestFailed =>
      log.debug("Request failed due to {}", failed)
      Task.now(fallback)
    case NoSuitablePeer =>
      Task.now(fallback).delayExecution(syncConfig.syncRetryInterval)
    case Failure(cause) =>
      log.error("Unexpected error on the request result", cause)
      Task.now(fallback)
    case PeersClient.Response(peer, msg) =>
      Task.now(AdaptedMessage(peer, msg))
  }
}

object HeadersFetcher {

  def apply(
             peersClient: ClassicActorRef,
             syncConfig: SyncConfig,
             supervisor: ActorRef[FetchCommand]): Behavior[HeadersFetcherCommand] =
    Behaviors.setup(context => new HeadersFetcher(peersClient, syncConfig, supervisor, context))

  sealed trait HeadersFetcherCommand
  final case class FetchHeaders(blockNumber: BigInt, amount: BigInt) extends HeadersFetcherCommand
  final case object RetryHeadersRequest extends HeadersFetcherCommand
  private final case class AdaptedMessage[T <: Message](peer: Peer, msg: T) extends HeadersFetcherCommand
}
