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

  class PeerMessageBus extends ActorEventBus {

    override type Event = MessageFromPeer
    override type Classifier = MessageClassifier

    //FIXME Remove this var
    private var subscriptions: Map[(Subscriber, PeerSelector), Set[Int]] = Map.empty

    override def subscribe(subscriber: ActorRef, to: Classifier): Boolean = {
      val newSubscriptions = subscriptions.get((subscriber, to.peerSelector)) match {
        case Some(messageCodes) =>
          subscriptions + ((subscriber, to.peerSelector) -> (messageCodes ++ to.messageCodes))
        case None =>  subscriptions + ((subscriber, to.peerSelector) -> to.messageCodes)
      }
      if(newSubscriptions == subscriptions) false
      else {
        subscriptions = newSubscriptions
        true
      }
    }

    override def unsubscribe(subscriber: ActorRef, from: Classifier): Boolean = {
      subscriptions.get((subscriber, from.peerSelector)).exists { messageCodes =>
        val newMessageCodes = messageCodes -- from.messageCodes
        if (messageCodes == newMessageCodes) false
        else {
          if(newMessageCodes.isEmpty) subscriptions = subscriptions - ((subscriber, from.peerSelector))
          else subscriptions = subscriptions + ((subscriber, from.peerSelector) -> newMessageCodes)
          true
        }
      }
    }

    override def unsubscribe(subscriber: ActorRef): Unit = subscriptions = subscriptions.filterKeys(_._1 != subscriber)

    override def publish(event: MessageFromPeer): Unit = {
      subscriptions.flatMap { sub =>
        val ((subscriber, peerSelector), messageCodes) = sub
        if (peerSelector.contains(event.peerId) && messageCodes.contains(event.message.code)) Some(subscriber)
        else None
      }.toSeq.distinct.foreach( _ ! event )
    }

  }

  case class Subscribe(to: MessageClassifier)

  object Unsubscribe {
    def apply(): Unsubscribe = Unsubscribe(None)

    def apply(from: MessageClassifier): Unsubscribe = Unsubscribe(Some(from))
  }

  case class Unsubscribe(from: Option[MessageClassifier] = None)

  case class Publish(ev: MessageFromPeer)

}

class PeerMessageBusActor extends Actor {

  import PeerMessageBusActor._

  val peerMessageBus: PeerMessageBus = new PeerMessageBus

  override def receive: Receive = {
    case Subscribe(to) => peerMessageBus.subscribe(sender(), to)
    case Unsubscribe(Some(from)) => peerMessageBus.unsubscribe(sender(), from)
    case Unsubscribe(None) => peerMessageBus.unsubscribe(sender())
    case Publish(ev: MessageFromPeer) => peerMessageBus.publish(ev)
  }

}
