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

    //FIXME Remove both var
    private var messageSubscriptions: Map[(Subscriber, PeerSelector), Set[Int]] = Map.empty
    private var connectionSubscriptions: Seq[Subscription] = Nil

    /**
      * Subscribes the subscriber to a requested event
      *
      * @param subscriber
      * @param to, classifier for the event subscribed
      * @return true if successful and false if not (because it was already subscribed to that Classifier, or otherwise)
      */
    override def subscribe(subscriber: ActorRef, to: Classifier): Boolean = to match {
      case msgClassifier: MessageClassifier => subscribeToMessageReceived(subscriber, msgClassifier)
      case _ => subscribeToConnectionEvent(subscriber, to)
    }

    /**
      * Unsubscribes the subscriber from a requested event
      *
      * @param subscriber
      * @param from, classifier for the event to unsubscribe
      * @return true if successful and false if not (because it wasn't subscribed to that Classifier, or otherwise)
      */
    override def unsubscribe(subscriber: ActorRef, from: Classifier): Boolean = from match {
      case msgClassifier: MessageClassifier => unsubscribeFromMessageReceived(subscriber, msgClassifier)
      case _ => unsubscribeFromConnectionEvent(subscriber, from)
    }

    /**
      * Unsubscribes the subscriber from all events it was subscribed
      *
      * @param subscriber
      */
    override def unsubscribe(subscriber: ActorRef): Unit = {
      messageSubscriptions = messageSubscriptions.filterKeys(_._1 != subscriber)
      connectionSubscriptions = connectionSubscriptions.filterNot(_.subscriber == subscriber)
    }

    override def publish(event: PeerEvent): Unit = {
      val interestedSubscribers = event match {
        case MessageFromPeer(message, peerId) =>
          messageSubscriptions.flatMap { sub =>
            val ((subscriber, peerSelector), messageCodes) = sub
            if (peerSelector.contains(peerId) && messageCodes.contains(message.code)) Some(subscriber)
            else None
          }.toSeq.distinct
        case PeerDisconnected(peerId) =>
          connectionSubscriptions.collect {
            case Subscription(subscriber, classifier: PeerDisconnectedClassifier)
              if classifier.peerSelector.contains(peerId) => subscriber
          }
        case _: PeerHandshakeSuccessful[_] =>
          connectionSubscriptions.collect {
            case Subscription(subscriber, PeerHandshaked) => subscriber
          }
      }
      interestedSubscribers.foreach(_ ! event)
    }

    /**
      * Subscribes the subscriber to a requested message received event
      *
      * @param subscriber
      * @param to, classifier for the message received event subscribed
      * @return true if successful and false if not (because it was already subscribed to that Classifier, or otherwise)
      */
    private def subscribeToMessageReceived(subscriber: ActorRef, to: MessageClassifier): Boolean = {
      val newSubscriptions = messageSubscriptions.get((subscriber, to.peerSelector)) match {
        case Some(messageCodes) =>
          messageSubscriptions + ((subscriber, to.peerSelector) -> (messageCodes ++ to.messageCodes))
        case None =>  messageSubscriptions + ((subscriber, to.peerSelector) -> to.messageCodes)
      }
      if(newSubscriptions == messageSubscriptions) false
      else {
        messageSubscriptions = newSubscriptions
        true
      }
    }

    /**
      * Subscribes the subscriber to a requested connection event (new peer handshaked or peer disconnected)
      *
      * @param subscriber
      * @param to, classifier for the connection event subscribed
      * @return true if successful and false if not (because it was already subscribed to that Classifier, or otherwise)
      */
    private def subscribeToConnectionEvent(subscriber: ActorRef, to: Classifier): Boolean = {
      val subscription = Subscription(subscriber, to)
      if (connectionSubscriptions.contains(subscription)) {
        false
      } else {
        connectionSubscriptions = connectionSubscriptions :+ subscription
        true
      }
    }

    /**
      * Unsubscribes the subscriber from a requested received message event event
      *
      * @param subscriber
      * @param from, classifier for the message received event to unsubscribe
      * @return true if successful and false if not (because it wasn't subscribed to that Classifier, or otherwise)
      */
    private def unsubscribeFromMessageReceived(subscriber: ActorRef, from: MessageClassifier): Boolean = {
      messageSubscriptions.get((subscriber, from.peerSelector)).exists { messageCodes =>
        val newMessageCodes = messageCodes -- from.messageCodes
        if (messageCodes == newMessageCodes) false
        else {
          if(newMessageCodes.isEmpty) messageSubscriptions = messageSubscriptions - ((subscriber, from.peerSelector))
          else messageSubscriptions = messageSubscriptions + ((subscriber, from.peerSelector) -> newMessageCodes)
          true
        }
      }
    }

    /**
      * Unsubscribes the subscriber from a requested event
      *
      * @param subscriber
      * @param from, classifier for the connection event to unsubscribe
      * @return true if successful and false if not (because it wasn't subscribed to that Classifier, or otherwise)
      */
    private def unsubscribeFromConnectionEvent(subscriber: ActorRef, from: Classifier): Boolean = {
      val subscription = Subscription(subscriber, from)
      if (connectionSubscriptions.contains(subscription)) {
        connectionSubscriptions = connectionSubscriptions.filterNot(_ == subscription)
        true
      } else {
        false
      }
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
