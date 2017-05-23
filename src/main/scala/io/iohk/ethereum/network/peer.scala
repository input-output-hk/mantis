package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import io.iohk.ethereum.network.PeerActor.{DisconnectPeer, SendMessage}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, SubscriptionClassifier, Unsubscribe}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPEncoder

trait Peer {

  /**
    * Unique identifier of the peer
    */
  val id: PeerId

  /**
    * Sends a message to the peer
    *
    * @param message to send to the peer
    */
  def send[M <: Message](message: M)(implicit enc: RLPEncoder[M]): Unit

  /**
    * Sends various messages of the same type to the peer
    *
    * @param messages to send to the peer
    */
  def send[M <: Message](messages: Seq[M])(implicit enc: RLPEncoder[M]): Unit =
    messages.foreach(msg => send(msg)(enc))

  /**
    * Ends the connection with the peer
    *
    * @param reason to disconnect from the peer
    */
  def disconnectFromPeer(reason: Int): Unit

  def subscribeToSetOfMsgs(msgs: Set[Int])(implicit subscriber: ActorRef): Unit
  def unsubscribeFromSetOfMsgs(msgs: Set[Int])(implicit subscriber: ActorRef): Unit

  /**
    * Subscribes the actor sender to the event of the peer disconnecting.
    * The subscriber will receive a [[io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected]] when the
    * peer is disconnected.
    *
    * @param subscriber, the sender of the subscription
    */
  def subscribeToDisconnect()(implicit subscriber: ActorRef): Unit

  /**
    * Unsubscribes the actor sender to the event of the peer disconnecting
    *
    * @param subscriber, the sender of the unsubscription
    */
  def unsubscribeFromDisconnect()(implicit subscriber: ActorRef): Unit

}

case class PeerId(value: String) extends AnyVal

case class PeerImpl(remoteAddress: InetSocketAddress, ref: ActorRef, peerEventBusActor: ActorRef) extends Peer {
  val id: PeerId = PeerId(ref.path.name)

  override def send[M <: Message](message: M)(implicit enc: RLPEncoder[M]): Unit = {
    ref ! SendMessage(message)
  }

  override def disconnectFromPeer(reason: Int): Unit =
    ref ! DisconnectPeer(reason)

  override def subscribeToSetOfMsgs(msgsCodes: Set[Int])(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.tell(Subscribe(MessageClassifier(msgsCodes, PeerSelector.WithId(id))), subscriber)

  override def unsubscribeFromSetOfMsgs(msgsCodes: Set[Int])(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.tell(Unsubscribe(MessageClassifier(msgsCodes, PeerSelector.WithId(id))), subscriber)

  override def subscribeToDisconnect()(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.tell(Subscribe(SubscriptionClassifier.PeerDisconnection(id)), subscriber)

  def unsubscribeFromDisconnect()(implicit subscriber: ActorRef): Unit =
    peerEventBusActor.tell(Unsubscribe(SubscriptionClassifier.PeerDisconnection(id)), subscriber)
}
