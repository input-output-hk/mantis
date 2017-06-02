package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor.{ActorRef, ActorSystem}
import akka.agent.Agent
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerHandshaked}
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.PeerManagerActor.{ConnectToPeer, GetPeers, PeerConfiguration, Peers}
import akka.pattern.ask
import akka.util.Timeout
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.ServerActor.StartServer
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.utils.NodeStatus

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

trait Network {

  /**
    * Returns all the current peers with their respective status
    */
  def peers(): Future[Peers]

  /**
    * Connects to a new peer
    *
    * @param uri of the new peer
    */
  def connect(uri: URI): Unit

  /**
    * Starts listening to new connections if it didn't before
    *
    * @param listenAddress, to which the network will listen to new connections
    */
  def listen(listenAddress: InetSocketAddress): Unit

  /**
    * Subscribes the actor sender to the event of any peer sending any message from a given set.
    * The subscriber will receive a [[io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer]] when any of
    * those messages are sent from any of the peers.
    *
    * @param subscriber, the sender of the subscription
    */
  def subscribe(messageCodes: Set[Int])(implicit subscriber: ActorRef): Unit

  /**
    * Unsubscribes the actor sender to the event of any peer sending any message from a given set
    *
    * @param subscriber, the sender of the unsubscription
    */
  def unsubscribe(messageCodes: Set[Int])(implicit subscriber: ActorRef): Unit

  /**
    * Subscribes the actor sender to the event of any peer successfully finishing the handshake.
    * The subscriber will receive a [[io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerHandshakeSuccessful]]
    * when any peer successfully finishes the handshaking.
    *
    * @param subscriber, the sender of the subscription
    */
  def subscribeToAnyPeerHandshaked()(implicit subscriber: ActorRef): Unit

  /**
    * Unsubscribes the actor sender to the event any peer successfully finishing the handshake
    *
    * @param subscriber, the sender of the unsubscription
    */
  def unsubscribeFromAnyPeerHandshaked()(implicit subscriber: ActorRef): Unit

  /**
    * Sends a given message to all peers in the network
    *
    * @param message to be sent to all peers
    */
  def broadcast(message: MessageSerializable): Unit

}

class NetworkImpl private[network] (peerManagerActor: ActorRef, peerEventBusActor: ActorRef,
                                    serverActor: ActorRef) extends Network {

  implicit val timeout = Timeout(3.seconds)

  def peers(): Future[Peers] = (peerManagerActor ? GetPeers).mapTo[Peers]

  def connect(uri: URI): Unit = peerManagerActor ! ConnectToPeer(uri)

  def listen(listenAddress: InetSocketAddress): Unit = serverActor ! StartServer(listenAddress)

  def subscribe(messageCodes: Set[Int])(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.!(Subscribe(MessageClassifier(messageCodes, PeerSelector.AllPeers)))( subscriber)

  def unsubscribe(messageCodes: Set[Int])(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.!(Unsubscribe(MessageClassifier(messageCodes, PeerSelector.AllPeers)))(subscriber)

  def subscribeToAnyPeerHandshaked()(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.!(Subscribe(PeerHandshaked))(subscriber)

  def unsubscribeFromAnyPeerHandshaked()(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.!(Unsubscribe(PeerHandshaked))(subscriber)

  def broadcast(message: MessageSerializable): Unit =
    peers().foreach{ _.peers.keys.foreach( _.send(message)) }

}

object NetworkImpl {

  //FIXME: Some of this parameters will be later removed (dependent on open PRs)
  def apply(nodeStatusHolder: Agent[NodeStatus], peerConfiguration: PeerConfiguration,
            appStateStorage: AppStateStorage, blockchain: Blockchain, bootstrapNodes: Set[String],
            forkResolverOpt: Option[ForkResolver], handshaker: Handshaker[EtcPeerInfo])
           (implicit actorSystem: ActorSystem): NetworkImpl = {

    val peerEventBusActor: ActorRef = actorSystem.actorOf(PeerEventBusActor.props, "peer-event-bus")

    val peerManagerActor: ActorRef = actorSystem.actorOf(PeerManagerActor.props(
      nodeStatusHolder,
      peerConfiguration,
      appStateStorage,
      blockchain,
      bootstrapNodes,
      peerEventBusActor,
      forkResolverOpt,
      handshaker), "peer-manager")

    val serverActor = actorSystem.actorOf(ServerActor.props(nodeStatusHolder, peerManagerActor), "server")

    new NetworkImpl(peerManagerActor, peerEventBusActor, serverActor)

  }

}
