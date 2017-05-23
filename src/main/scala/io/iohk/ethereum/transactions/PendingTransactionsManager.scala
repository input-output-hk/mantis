package io.iohk.ethereum.transactions

import akka.actor.{Actor, ActorRef, Props}
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.{Peer, PeerActor, PeerId, PeerManagerActor}
import io.iohk.ethereum.network.PeerMessageBusActor.{MessageClassifier, MessageFromPeer, PeerSelector, Subscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object PendingTransactionsManager {
  def props(peerManager: ActorRef, peerMessageBus: ActorRef): Props =
    Props(new PendingTransactionsManager(peerManager, peerMessageBus))

  case class AddTransaction(signedTransaction: SignedTransaction)
  private case class NotifyPeer(signedTransactions: Seq[SignedTransaction], peer: Peer)

  case object GetPendingTransactions
  case class PendingTransactions(signedTransactions: Seq[SignedTransaction])

  case class RemoveTransactions(signedTransactions: Seq[SignedTransaction])
}

class PendingTransactionsManager(peerManager: ActorRef, peerMessageBus: ActorRef) extends Actor {

  import PendingTransactionsManager._
  import akka.pattern.ask

  /**
    * stores all pending transactions
   */
  var pendingTransactions: Seq[SignedTransaction] = Nil

  /**
    * stores information which tx hashes are "known" by which peers
    */
  var knownTransactions: Map[ByteString, Set[PeerId]] = Map.empty

  implicit val timeout = Timeout(3.seconds)

  peerMessageBus ! Subscribe(MessageClassifier(Set(SignedTransactions.code), PeerSelector.AllPeers))

  override def receive: Receive = {
    case AddTransaction(signedTransaction) =>
      // TODO: we should check whether a transaction with the same header and nonce exists, and if so replace it with the
      // new version
      if (!pendingTransactions.contains(signedTransaction)) {
        pendingTransactions :+= signedTransaction
        (peerManager ? PeerManagerActor.GetPeers).mapTo[Peers].foreach { peers =>
          peers.handshaked.foreach { case (peer, _) => self ! NotifyPeer(Seq(signedTransaction), peer) }
        }
      }

    case NotifyPeer(signedTransactions, peer) =>
      val txsToNotify = signedTransactions
        .filter(pendingTransactions.contains) // signed transactions that are still pending
        .filterNot(isTxKnown(_, peer.id)) // and not known by peer

        if (txsToNotify.nonEmpty) {
          peer.ref ! PeerActor.SendMessage(SignedTransactions(txsToNotify))
          txsToNotify.foreach(setTxKnown(_, peer.id))
        }

    case GetPendingTransactions =>
      sender() ! PendingTransactions(pendingTransactions)

    case RemoveTransactions(signedTransactions) =>
      pendingTransactions = pendingTransactions.filterNot(signedTransactions.contains)
      knownTransactions = knownTransactions.filterNot(signedTransactions.map(_.hash).contains)

    case MessageFromPeer(SignedTransactions(signedTransactions), peerId) =>
      pendingTransactions ++= signedTransactions
      signedTransactions.foreach(setTxKnown(_, peerId))
      (peerManager ? PeerManagerActor.GetPeers).mapTo[Peers].foreach { peers =>
        peers.handshaked.foreach { case (peer, _) => self ! NotifyPeer(signedTransactions, peer) }
      }
  }

  private def isTxKnown(signedTransaction: SignedTransaction, peerId: PeerId): Boolean =
    knownTransactions.getOrElse(signedTransaction.hash, Set.empty).contains(peerId)

  private def setTxKnown(signedTransaction: SignedTransaction, peerId: PeerId): Unit = {
    val currentPeers = knownTransactions.getOrElse(signedTransaction.hash, Set.empty)
    val newPeers = currentPeers + peerId
    knownTransactions += (signedTransaction.hash -> newPeers)
  }

}
