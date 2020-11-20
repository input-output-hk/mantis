package io.iohk.ethereum.network.discovery

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.pipe
import akka.util.ByteString
import cats.effect.Resource
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.scalanet.discovery.ethereum.v4
import io.iohk.scalanet.discovery.ethereum.{Node => ENode}
import monix.eval.Task
import monix.execution.{Scheduler, BufferCapacity}
import monix.tail.Iterant
import monix.catnap.ConsumerF
import scala.util.{Failure, Success}
import scala.util.Random

class PeerDiscoveryManager(
    localNodeId: ByteString,
    discoveryConfig: DiscoveryConfig,
    knownNodesStorage: KnownNodesStorage,
    // The manager only starts the DiscoveryService if discovery is enabled.
    discoveryServiceResource: Resource[Task, v4.DiscoveryService],
    randomNodeBufferSize: Int
)(implicit scheduler: Scheduler)
    extends Actor
    with ActorLogging {

  // Create a random nodes iterator on top of the service so the node can quickly ramp up its peers.
  val discoveryResources = for {
    service <- discoveryServiceResource

    randomNodes = Iterant
      .repeatEvalF {
        Task(log.debug("Pulling random nodes on demand...")) >>
          service.lookupRandom
      }
      .flatMap(ns => Iterant.fromList(ns.toList))
      .map(toNode)
      .filter(!isLocalNode(_))

    randomNodeConsumer <- randomNodes.consumeWithConfig(
      // Using bounded capacity for back pressure, so we don't make more lookups unless there is a need.
      ConsumerF.Config(capacity = Some(BufferCapacity.Bounded(randomNodeBufferSize)))
    )
  } yield (service, randomNodeConsumer)

  import PeerDiscoveryManager._

  // The following logic is for backwards compatibility.
  val alreadyDiscoveredNodes: Vector[Node] =
    if (!discoveryConfig.reuseKnownNodes) Vector.empty
    else {
      // The manager considered the bootstrap nodes discovered, even if discovery was disabled.
      val bootstrapNodes: Set[Node] =
        discoveryConfig.bootstrapNodes
      // The known nodes were considered discovered even if they haven't yet responded to pings; unless discovery was disabled.
      val knownNodes: Set[Node] =
        if (!discoveryConfig.discoveryEnabled) Set.empty
        else
          knownNodesStorage.getKnownNodes().map(Node.fromUri)

      (bootstrapNodes ++ knownNodes).filterNot(isLocalNode).toVector
    }

  override def receive: Receive = init

  private def handleNodeInfoRequests(discovery: Option[Discovery]): Receive = {
    case GetDiscoveredNodesInfo =>
      sendDiscoveredNodesInfo(discovery.map(_._1), sender)

    case GetRandomNodeInfo =>
      sendRandomNodeInfo(discovery.map(_._2), sender)
  }

  // The service hasn't been started yet, so it just serves the static known nodes.
  def init: Receive = handleNodeInfoRequests(None) orElse {
    case Start =>
      if (discoveryConfig.discoveryEnabled) {
        log.info("Starting peer discovery...")
        startDiscoveryService()
        context.become(starting)
      } else {
        log.info("Peer discovery is disabled.")
      }

    case Stop =>
  }

  // Waiting for the DiscoveryService to be initialized. Keep serving known nodes.
  // This would not be needed if Actors were treated as resources themselves.
  def starting: Receive = handleNodeInfoRequests(None) orElse {
    case Start =>

    case Stop =>
      log.info("Stopping peer discovery...")
      context.become(stopping)

    case StartAttempt(result) =>
      result match {
        case Right((discovery, release)) =>
          log.info("Peer discovery started.")
          context.become(started(discovery, release))

        case Left(ex) =>
          log.error(ex, "Failed to start peer discovery.")
          context.become(init)
      }
  }

  // DiscoveryService started, we can ask it for nodes now.
  def started(discovery: Discovery, release: Task[Unit]): Receive =
    handleNodeInfoRequests(Some(discovery)) orElse {
      case Start =>

      case Stop =>
        log.info("Stopping peer discovery...")
        stopDiscoveryService(release)
        context.become(stopping)
    }

  // Waiting for the DiscoveryService to be initialized OR we received a stop request
  // before it even got a chance to start, so we'll stop it immediately.
  def stopping: Receive = handleNodeInfoRequests(None) orElse {
    case Start | Stop =>

    case StartAttempt(result) =>
      result match {
        case Right((_, release)) =>
          log.info("Peer discovery started, now stopping...")
          stopDiscoveryService(release)

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

  def startDiscoveryService(): Unit = {
    discoveryResources.allocated.runToFuture
      .onComplete {
        case Failure(ex) =>
          self ! StartAttempt(Left(ex))
        case Success(result) =>
          self ! StartAttempt(Right(result))
      }
  }

  def stopDiscoveryService(release: Task[Unit]): Unit = {
    release.runToFuture.onComplete {
      case Failure(ex) =>
        self ! StopAttempt(Left(ex))
      case Success(result) =>
        self ! StopAttempt(Right(result))
    }
  }

  def sendDiscoveredNodesInfo(
      maybeDiscoveryService: Option[v4.DiscoveryService],
      recipient: ActorRef
  ): Unit = pipeToRecipient(recipient) {

    val maybeDiscoveredNodes: Task[Set[Node]] =
      maybeDiscoveryService.fold(Task.pure(Set.empty[Node])) {
        _.getNodes.map { nodes =>
          nodes.map(toNode)
        }
      }

    maybeDiscoveredNodes
      .map(_ ++ alreadyDiscoveredNodes)
      .map(_.filterNot(isLocalNode))
      .map(DiscoveredNodesInfo(_))
  }

  def sendRandomNodeInfo(
      maybeRandomNodes: Option[RandomNodes],
      recipient: ActorRef
  ): Unit = pipeToRecipient[RandomNodeInfo](recipient) {
    val randomNode = maybeRandomNodes match {
      case None if alreadyDiscoveredNodes.isEmpty =>
        Task.raiseError(
          new IllegalStateException("There are no known nodes and discovery isn't running.")
        )

      case None =>
        Task.pure(alreadyDiscoveredNodes(Random.nextInt(alreadyDiscoveredNodes.size)))

      case Some(consumer) =>
        consumer.pull
          .flatMap {
            case Left(None) =>
              Task.raiseError(new IllegalStateException("The random node source is finished."))
            case Left(Some(ex)) =>
              Task.raiseError(ex)
            case Right(node) =>
              Task.pure(node)
          }
    }
    randomNode.map(RandomNodeInfo(_))
  }

  def pipeToRecipient[T](recipient: ActorRef)(task: Task[T]): Unit =
    task
      .doOnFinish {
        _.fold(Task.unit)(ex => Task(log.error(ex, "Failed to relay result to recipient.")))
      }
      .runToFuture
      .pipeTo(recipient)

  def toNode(enode: ENode): Node =
    Node(
      id = ByteString(enode.id.toByteArray),
      addr = enode.address.ip,
      tcpPort = enode.address.tcpPort,
      udpPort = enode.address.udpPort
    )

  def isLocalNode(node: Node): Boolean =
    node.id == localNodeId
}

object PeerDiscoveryManager {
  def props(
      localNodeId: ByteString,
      discoveryConfig: DiscoveryConfig,
      knownNodesStorage: KnownNodesStorage,
      discoveryServiceResource: Resource[Task, v4.DiscoveryService],
      randomNodeBufferSize: Int = 0
  )(implicit scheduler: Scheduler): Props =
    Props(
      new PeerDiscoveryManager(
        localNodeId,
        discoveryConfig,
        knownNodesStorage,
        discoveryServiceResource,
        randomNodeBufferSize = math.max(randomNodeBufferSize, discoveryConfig.kademliaBucketSize)
      )
    )

  case object Start
  case object Stop

  // Iterate over random lookups.
  private type RandomNodes = Iterant.Consumer[Task, Node]
  private type Discovery = (v4.DiscoveryService, RandomNodes)

  private case class StartAttempt(
      result: Either[Throwable, (Discovery, Task[Unit])]
  )
  private case class StopAttempt(result: Either[Throwable, Unit])

  /** Get all nodes discovered so far. */
  case object GetDiscoveredNodesInfo
  case class DiscoveredNodesInfo(nodes: Set[Node])

  /** Return the next peer from a series of random lookups. */
  case object GetRandomNodeInfo
  case class RandomNodeInfo(node: Node)
}
