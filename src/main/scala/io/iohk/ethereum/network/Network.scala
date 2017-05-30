package io.iohk.ethereum.network

import akka.actor.ActorRef
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerHandshaked}
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peers}
import akka.pattern.ask
import akka.util.Timeout
import io.iohk.ethereum.network.p2p.MessageSerializable
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scala.concurrent.duration._

trait Network {

  /**
    * Returns all the current peers with their respective status
    */
  def peersWithStatus(): Future[Peers]

  /**
    * Subscribes the actor sender to the event of any peer sending any message from a given set.
    * The subscriber will receive a [[io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer]] when any of
    * those messages are sent from any of the peers.
    *
    * @param subscriber, the sender of the subscription
    */
  def subscribeToSetOfMsgs(messageCodes: Set[Int])(implicit subscriber: ActorRef): Unit

  /**
    * Unsubscribes the actor sender to the event of any peer sending any message from a given set
    *
    * @param subscriber, the sender of the unsubscription
    */
  def unsubscribeFromSetOfMsgs(messageCodes: Set[Int])(implicit subscriber: ActorRef): Unit

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

case class NetworkImpl(peerManagerActor: ActorRef, peerEventBusActor: ActorRef) extends Network {

  implicit val timeout = Timeout(3.seconds)

  def peersWithStatus(): Future[Peers] = (peerManagerActor ? GetPeers).mapTo[Peers]

  def subscribeToSetOfMsgs(messageCodes: Set[Int])(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.!(Subscribe(MessageClassifier(messageCodes, PeerSelector.AllPeers)))( subscriber)

  def unsubscribeFromSetOfMsgs(messageCodes: Set[Int])(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.!(Unsubscribe(MessageClassifier(messageCodes, PeerSelector.AllPeers)))(subscriber)

  def subscribeToAnyPeerHandshaked()(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.!(Subscribe(PeerHandshaked))(subscriber)

  def unsubscribeFromAnyPeerHandshaked()(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.!(Unsubscribe(PeerHandshaked))(subscriber)

  def broadcast(message: MessageSerializable): Unit =
    peersWithStatus().foreach{ _.peers.keys.foreach( _.send(message)) }

}
