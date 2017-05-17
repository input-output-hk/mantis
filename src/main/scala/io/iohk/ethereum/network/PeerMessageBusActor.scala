package io.iohk.ethereum.network

import akka.actor.{Actor, ActorRef, Props}
import akka.event.ActorEventBus
import io.iohk.ethereum.network.p2p.Message

object PeerMessageBusActor {
  def props: Props = Props(new PeerMessageBusActor)

  sealed trait PeerSelector {
    def contains(peerId: PeerId): Boolean
  }

  object PeerSelector {

    case object AllPeers extends PeerSelector {
      override def contains(p: PeerId): Boolean = true
    }

    case class WithId(peerId: PeerId) extends PeerSelector {
      override def contains(p: PeerId): Boolean = p == peerId
    }

  }

  case class MessageClassifier(messageCodes: Set[Int], peerSelector: PeerSelector)

  case class MessageFromPeer(message: Message, peerId: PeerId)

  case class Subscription(subscriber: ActorRef, classifier: MessageClassifier)

  class PeerMessageBus extends ActorEventBus {

    override type Event = MessageFromPeer
    override type Classifier = MessageClassifier

    private var subscriptions: Seq[Subscription] = Nil

    override def subscribe(subscriber: ActorRef, to: Classifier): Boolean = {
      val subscription = Subscription(subscriber, to)
      if (subscriptions.contains(subscription)) {
        false
      } else {
        subscriptions = subscriptions :+ subscription
        true
      }
    }

    override def unsubscribe(subscriber: ActorRef, from: Classifier): Boolean = {
      val subscription = Subscription(subscriber, from)
      if (subscriptions.contains(subscription)) {
        subscriptions = subscriptions.filterNot(_ == subscription)
        true
      } else {
        false
      }
    }

    override def unsubscribe(subscriber: ActorRef): Unit = {
      subscriptions = subscriptions.filterNot(_.subscriber == subscriber)
    }

    override def publish(event: MessageFromPeer): Unit = {
      subscriptions.filter(_.classifier.peerSelector.contains(event.peerId)).foreach(_.subscriber ! event)
    }

  }

  case class Subscribe(to: MessageClassifier)
  case class Unsubscribe(from: MessageClassifier)
  case class Publish(ev: MessageFromPeer)
}

class PeerMessageBusActor extends Actor {

  import PeerMessageBusActor._

  val peerMessageBus: PeerMessageBus = new PeerMessageBus

  override def receive: Receive = {
    case Subscribe(to) => peerMessageBus.subscribe(sender(), to)
    case Unsubscribe(from) => peerMessageBus.unsubscribe(sender(), from)
    case Publish(ev: MessageFromPeer) => peerMessageBus.publish(ev)
  }

}
