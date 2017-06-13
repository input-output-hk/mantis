package io.iohk.ethereum.network

import akka.actor.{Actor, ActorRef, Props}
import akka.event.ActorEventBus
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerDisconnected, PeerHandshakeSuccessful}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier._
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeResult
import io.iohk.ethereum.network.p2p.Message

object PeerEventBusActor {
  def props: Props = Props(new PeerEventBusActor)

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

  sealed trait SubscriptionClassifier

  object SubscriptionClassifier {
    case class MessageClassifier(messageCodes: Set[Int], peerSelector: PeerSelector) extends SubscriptionClassifier
    case class PeerDisconnectedClassifier(peerSelector: PeerSelector) extends SubscriptionClassifier
    case object PeerHandshaked extends SubscriptionClassifier
  }

  sealed trait PeerEvent

  object PeerEvent{
    case class MessageFromPeer(message: Message, peerId: PeerId) extends PeerEvent
    case class PeerDisconnected(peerId: PeerId) extends PeerEvent
    case class PeerHandshakeSuccessful[R <: HandshakeResult](peer: Peer, handshakeResult: R) extends PeerEvent
  }

  case class Subscription(subscriber: ActorRef, classifier: SubscriptionClassifier)

  class PeerEventBus extends ActorEventBus {

    override type Event = PeerEvent
    override type Classifier = SubscriptionClassifier

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

    override def publish(event: PeerEvent): Unit = {
      val interestedSubscribers = event match {
        case MessageFromPeer(message, peerId) =>
          subscriptions.collect {
            case Subscription(subscriber, classifier: MessageClassifier)
              if classifier.peerSelector.contains(peerId) &&
                classifier.messageCodes.contains(message.code) => subscriber
          }
        case PeerDisconnected(peerId) =>
          subscriptions.collect {
            case Subscription(subscriber, classifier: PeerDisconnectedClassifier)
              if classifier.peerSelector.contains(peerId) => subscriber
          }
        case _: PeerHandshakeSuccessful[_] =>
          subscriptions.collect {
            case Subscription(subscriber, PeerHandshaked) => subscriber
          }
      }
      interestedSubscribers.foreach(_ ! event)
    }

  }

  case class Subscribe(to: SubscriptionClassifier)
  object Unsubscribe {
    def apply(): Unsubscribe = Unsubscribe(None)
    def apply(from: SubscriptionClassifier): Unsubscribe = Unsubscribe(Some(from))
  }
  case class Unsubscribe(from: Option[SubscriptionClassifier] = None)
  case class Publish(ev: PeerEvent)
}

class PeerEventBusActor extends Actor {

  import PeerEventBusActor._

  val peerEventBus: PeerEventBus = new PeerEventBus

  override def receive: Receive = {
    case Subscribe(to) => peerEventBus.subscribe(sender(), to)
    case Unsubscribe(Some(from)) => peerEventBus.unsubscribe(sender(), from)
    case Unsubscribe(None) => peerEventBus.unsubscribe(sender())
    case Publish(ev: PeerEvent) => peerEventBus.publish(ev)
  }

}
