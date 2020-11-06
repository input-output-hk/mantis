package io.iohk.ethereum.network.discovery

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.pipe
import akka.util.ByteString
import cats.effect.Resource
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.scalanet.discovery.ethereum.v4
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.control.NonFatal

class PeerDiscoveryManager(
    discoveryConfig: DiscoveryConfig,
    knownNodesStorage: KnownNodesStorage,
    // The manager only starts the DiscoveryService if discovery is enabled.
    discoveryServiceResource: Resource[Task, v4.DiscoveryService]
)(implicit scheduler: Scheduler)
    extends Actor
    with ActorLogging {

  import PeerDiscoveryManager._

  // The following logic is for backwards compatibility.
  val alreadyDiscoveredNodes: Set[Node] =
    if (!discoveryConfig.reuseKnownNodes) Set.empty
    else {
      // The manager considered the bootstrap nodes discovered, even if discovery was disabled.
      val bootstrapNodes: Set[Node] =
        discoveryConfig.bootstrapNodes
      // The known nodes were considered discovered even if they haven't yet responded to pings; unless discovery was disabled.
      val knownNodes: Set[Node] =
        if (!discoveryConfig.discoveryEnabled) Set.empty
        else
          knownNodesStorage.getKnownNodes().map(Node.fromUri)

      bootstrapNodes ++ knownNodes
    }

  override def receive: Receive = init

  // The service hasn't been started yet, so it just serves the static known nodes.
  def init: Receive = {
    case GetDiscoveredNodesInfo =>
      sendDiscoveredNodesInfo(None, sender)

    case Start =>
      if (discoveryConfig.discoveryEnabled) {
        log.info("Starting peer discovery...")

        discoveryServiceResource.allocated.attempt
          .map(StartAttempt)
          .runToFuture
          .pipeTo(self)

        context.become(starting)
      } else {
        log.info("Peer discovery is disabled.")
      }

    case Stop =>
  }

  // Waiting for the DiscoveryService to be initialized. Keep serving known nodes.
  // This would not be needed if Actors were treated as resources themselves.
  def starting: Receive = {
    case GetDiscoveredNodesInfo =>
      sendDiscoveredNodesInfo(None, sender)

    case Start =>

    case Stop =>
      log.info("Stopping peer discovery...")
      context.become(stopping)

    case StartAttempt(result) =>
      result match {
        case Right((service, release)) =>
          log.info("Peer discovery started.")
          context.become(started(service, release))

        case Left(ex) =>
          log.error(ex, "Failed to start peer discovery.")
          context.become(init)
      }
  }

  // DiscoveryService started, we can ask it for nodes now.
  def started(service: v4.DiscoveryService, release: Task[Unit]): Receive = {
    case GetDiscoveredNodesInfo =>
      sendDiscoveredNodesInfo(Some(service), sender)

    case Start =>

    case Stop =>
      log.info("Stopping peer discovery...")
      release.attempt.map(StopAttempt).runToFuture.pipeTo(self)
      context.become(stopping)
  }

  // Waiting for the DiscoveryService to be initialized OR we received a stop request
  // before it even got a chance to start, so we'll stop it immediately.
  def stopping: Receive = {
    case GetDiscoveredNodesInfo =>
      sendDiscoveredNodesInfo(None, sender)

    case Start | Stop =>

    case StartAttempt(result) =>
      result match {
        case Right((_, release)) =>
          log.info("Peer discovery started, now stopping...")
          release.attempt.map(StopAttempt).runToFuture.pipeTo(self)

        case Left(ex) =>
          log.error(ex, "Failed to start peer discovery.")
          context.become(init)
      }

    case StopAttempt(result) =>
      result match {
        case Right(_) =>
          log.info("Peer discovery stopped.")
        case Left(ex) =>
          log.error(ex, "Failed to stop peer discovery.")
      }
      context.become(init)
  }

  def sendDiscoveredNodesInfo(
      maybeDiscoveryService: Option[v4.DiscoveryService],
      recipient: ActorRef
  ): Unit = {

    val maybeDiscoveredNodes: Task[Set[Node]] =
      maybeDiscoveryService.fold(Task.pure(Set.empty[Node])) {
        _.getNodes.map { nodes =>
          nodes.map { node =>
            Node(
              id = ByteString(node.id.toByteArray),
              addr = node.address.ip,
              tcpPort = node.address.tcpPort,
              udpPort = node.address.udpPort
            )
          }
        }
      }

    maybeDiscoveredNodes
      .map(_ ++ alreadyDiscoveredNodes)
      .map(DiscoveredNodesInfo(_))
      .doOnFinish {
        case Some(NonFatal(ex)) =>
          Task(log.error(ex, "Failed to get discovered nodes."))
        case _ =>
          Task.unit
      }
      .runToFuture
      .pipeTo(recipient)
  }
}

object PeerDiscoveryManager {
  def props(
      discoveryConfig: DiscoveryConfig,
      knownNodesStorage: KnownNodesStorage,
      discoveryServiceResource: Resource[Task, v4.DiscoveryService]
  )(implicit scheduler: Scheduler): Props =
    Props(
      new PeerDiscoveryManager(
        discoveryConfig,
        knownNodesStorage,
        discoveryServiceResource
      )
    )

  case object Start
  case object Stop

  private case class StartAttempt(result: Either[Throwable, (v4.DiscoveryService, Task[Unit])])
  private case class StopAttempt(result: Either[Throwable, Unit])

  case object GetDiscoveredNodesInfo
  case class DiscoveredNodesInfo(nodes: Set[Node])
}
