package io.iohk.ethereum.network

import java.net.URI

import akka.actor.ActorRef
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerHandshaked}
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peers}
import akka.pattern.ask
import akka.util.Timeout
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPEncoder

import scala.concurrent.{ExecutionContext, Future}
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

  //FIXME: Needed?
  def broadcast[M <: Message : RLPEncoder](message: M)(implicit executionContext: ExecutionContext): Unit

  //FIXME: Needed?
  def connectTo(uri: URI): Unit

  //FIXME: Needed?
  def disconnectFrom(peer: Peer, reason: Int): Unit

}

//FIXME: The MessageDecoder, Handshaker and MessageHandler should be configured here
//FIXME: PeerManagerActor y PeerMessageBusActor should be created here, maybe have a network builder?
case class NetworkImpl(peerManagerActor: ActorRef, peerEventBusActor: ActorRef) extends Network {

  implicit val timeout = Timeout(3.seconds)

  def peersWithStatus(): Future[Peers] = (peerManagerActor ? GetPeers).mapTo[Peers]

  def subscribeToSetOfMsgs(messageCodes: Set[Int])(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.tell(Subscribe(MessageClassifier(messageCodes, PeerSelector.AllPeers)), subscriber)

  def unsubscribeFromSetOfMsgs(messageCodes: Set[Int])(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.tell(Unsubscribe(MessageClassifier(messageCodes, PeerSelector.AllPeers)), subscriber)

  def subscribeToAnyPeerHandshaked()(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.!(Subscribe(PeerHandshaked))(subscriber)

  def unsubscribeFromAnyPeerHandshaked()(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.!(Unsubscribe(PeerHandshaked))(subscriber)

  def broadcast[M <: Message : RLPEncoder](message: M)(implicit executionContext: ExecutionContext): Unit =
    peersWithStatus().foreach{ _.peers.keys.foreach( _.send(message)) }

  def connectTo(uri: URI): Unit = peerManagerActor ! PeerManagerActor.ConnectToPeer(uri)

  def disconnectFrom(peer: Peer, reason: Int): Unit = peer.disconnectFromPeer(reason)

}
