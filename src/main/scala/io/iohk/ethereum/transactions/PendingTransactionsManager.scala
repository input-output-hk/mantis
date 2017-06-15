package io.iohk.ethereum.transactions

import akka.actor.{Actor, ActorRef, Props}
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerActor, PeerId, PeerManagerActor}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.utils.MiningConfig

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object PendingTransactionsManager {
  def props(miningConfig: MiningConfig, peerManager: ActorRef, etcPeerManager: ActorRef, peerMessageBus: ActorRef): Props =
    Props(new PendingTransactionsManager(miningConfig, peerManager, etcPeerManager, peerMessageBus))

  case class AddTransactions(signedTransactions: List[SignedTransaction])

  object AddTransactions{
    def apply(txs: SignedTransaction*): AddTransactions = AddTransactions(txs.toList)
  }

  private case class NotifyPeer(signedTransactions: Seq[SignedTransaction], peer: Peer)

  case object GetPendingTransactions
  case class PendingTransactions(signedTransactions: Seq[SignedTransaction])

  case class RemoveTransactions(signedTransactions: Seq[SignedTransaction])
}

class PendingTransactionsManager(miningConfig: MiningConfig, peerManager: ActorRef,
                                 etcPeerManager: ActorRef, peerMessageBus: ActorRef) extends Actor {

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
    case AddTransactions(signedTransactions) =>
      // TODO: we should check whether a transaction with the same header and nonce exists, and if so replace it with the
      // new version
      val transactionsToAdd = signedTransactions.filterNot(t => pendingTransactions.contains(t))
      if (transactionsToAdd.nonEmpty) {
        pendingTransactions = (pendingTransactions ++ transactionsToAdd).takeRight(miningConfig.txPoolSize)
        (peerManager ? PeerManagerActor.GetPeers).mapTo[Peers].foreach { peers =>
          peers.handshaked.foreach { peer => self ! NotifyPeer(transactionsToAdd, peer) }
        }
      }

    case NotifyPeer(signedTransactions, peer) =>
      val txsToNotify = signedTransactions
        .filter(pendingTransactions.contains) // signed transactions that are still pending
        .filterNot(isTxKnown(_, peer.id)) // and not known by peer

        if (txsToNotify.nonEmpty) {
          etcPeerManager ! EtcPeerManagerActor.SendMessage(SignedTransactions(txsToNotify), peer.id)
          txsToNotify.foreach(setTxKnown(_, peer.id))
        }

    case GetPendingTransactions =>
      sender() ! PendingTransactions(pendingTransactions)

    case RemoveTransactions(signedTransactions) =>
      pendingTransactions = pendingTransactions.filterNot(signedTransactions.contains)
      knownTransactions = knownTransactions.filterNot(signedTransactions.map(_.hash).contains)

    case MessageFromPeer(SignedTransactions(signedTransactions), peerId) =>
      pendingTransactions = (pendingTransactions ++ signedTransactions).takeRight(miningConfig.txPoolSize)
      signedTransactions.foreach(setTxKnown(_, peerId))
      (peerManager ? PeerManagerActor.GetPeers).mapTo[Peers].foreach { peers =>
        peers.handshaked.foreach { p => self ! NotifyPeer(signedTransactions, p) }
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
