package io.iohk.ethereum.transactions

import akka.actor.{Actor, ActorRef, Props}
import akka.util.{ByteString, Timeout}
import com.google.common.cache.{Cache, CacheBuilder, RemovalNotification}
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerEvent, PeerSelector, Subscribe, SubscriptionClassifier}
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId, PeerManagerActor}
import io.iohk.ethereum.utils.TxPoolConfig
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object PendingTransactionsManager {
  def props(txPoolConfig: TxPoolConfig, peerManager: ActorRef, etcPeerManager: ActorRef, peerMessageBus: ActorRef): Props =
    Props(new PendingTransactionsManager(txPoolConfig, peerManager, etcPeerManager, peerMessageBus))

  case class AddTransactions(signedTransactions: Set[SignedTransaction])

  object AddTransactions{
    def apply(txs: SignedTransaction*): AddTransactions = AddTransactions(txs.toSet)
  }

  case class AddOrOverrideTransaction(signedTransaction: SignedTransaction)

  private case class NotifyPeer(signedTransactions: Seq[SignedTransaction], peer: Peer)

  case object GetPendingTransactions
  case class PendingTransactionsResponse(pendingTransactions: Seq[PendingTransaction])

  case class RemoveTransactions(signedTransactions: Seq[SignedTransaction])

  case class PendingTransaction(stx: SignedTransaction, addTimestamp: Long)

  case object ClearPendingTransactions
}

class PendingTransactionsManager(txPoolConfig: TxPoolConfig, peerManager: ActorRef,
                                  etcPeerManager: ActorRef, peerEventBus: ActorRef) extends Actor {

  import PendingTransactionsManager._
  import akka.pattern.ask

  /**
    * stores information which tx hashes are "known" by which peers
    */
  var knownTransactions: Map[ByteString, Set[PeerId]] = Map.empty

  /**
    * stores all pending transactions
    */
  val pendingTransactions: Cache[ByteString, PendingTransaction] = CacheBuilder.newBuilder()
    .expireAfterWrite(txPoolConfig.transactionTimeout._1, txPoolConfig.transactionTimeout._2)
    .maximumSize(txPoolConfig.txPoolSize)
    .removalListener(
      (notification: RemovalNotification[ByteString, PendingTransaction]) => if (notification.wasEvicted()) {
        knownTransactions = knownTransactions.filterNot(_._1 == notification.getKey)
      }
    ).build()

  implicit val timeout = Timeout(3.seconds)

  peerEventBus ! Subscribe(SubscriptionClassifier.PeerHandshaked)
  peerEventBus ! Subscribe(MessageClassifier(Set(SignedTransactions.code), PeerSelector.AllPeers))

  // scalastyle:off method.length
  override def receive: Receive = {
    case PeerEvent.PeerHandshakeSuccessful(peer, _) =>
      pendingTransactions.cleanUp()
      val stxs = pendingTransactions.asMap().values().asScala.toSeq.map(_.stx)
      self ! NotifyPeer(stxs, peer)

    case AddTransactions(signedTransactions) =>
      pendingTransactions.cleanUp()
      val stxs = pendingTransactions.asMap().values().asScala.map(_.stx).toSet
      val transactionsToAdd = signedTransactions.diff(stxs)
      if (transactionsToAdd.nonEmpty) {
        val timestamp = System.currentTimeMillis()
        transactionsToAdd.foreach(t => pendingTransactions.put(t.hash, PendingTransaction(t, timestamp)))
        (peerManager ? PeerManagerActor.GetPeers).mapTo[Peers].foreach { peers =>
          peers.handshaked.foreach { peer => self ! NotifyPeer(transactionsToAdd.toSeq, peer) }
        }
      }

    case AddOrOverrideTransaction(newStx) =>
      pendingTransactions.cleanUp()
      val obsoleteTxs = pendingTransactions.asMap().asScala.filter(
        ptx => ptx._2.stx.senderAddress == newStx.senderAddress && ptx._2.stx.tx.nonce == newStx.tx.nonce
      )
      pendingTransactions.invalidateAll(obsoleteTxs.keys.asJava)

      val timestamp = System.currentTimeMillis()
      pendingTransactions.put(newStx.hash, PendingTransaction(newStx, timestamp))

      (peerManager ? PeerManagerActor.GetPeers).mapTo[Peers].foreach {
        peers => peers.handshaked.foreach { peer => self ! NotifyPeer(List(newStx), peer) }
      }

    case NotifyPeer(signedTransactions, peer) =>
      pendingTransactions.cleanUp()
      val txsToNotify = signedTransactions
        .filter(stx => pendingTransactions.asMap().containsKey(stx.hash)) // signed transactions that are still pending
        .filterNot(isTxKnown(_, peer.id)) // and not known by peer

      if (txsToNotify.nonEmpty) {
        etcPeerManager ! EtcPeerManagerActor.SendMessage(SignedTransactions(txsToNotify), peer.id)
        txsToNotify.foreach(setTxKnown(_, peer.id))
      }

    case GetPendingTransactions =>
      pendingTransactions.cleanUp()
      sender() ! PendingTransactionsResponse(pendingTransactions.asMap().asScala.values.toSeq)

    case RemoveTransactions(signedTransactions) =>
      pendingTransactions.invalidateAll(signedTransactions.map(_.hash).asJava)
      knownTransactions = knownTransactions -- signedTransactions.map(_.hash)

    case MessageFromPeer(SignedTransactions(signedTransactions), peerId) =>
      self ! AddTransactions(signedTransactions.toSet)
      signedTransactions.foreach(setTxKnown(_, peerId))

    case ClearPendingTransactions =>
      pendingTransactions.invalidateAll()
  }

  private def isTxKnown(signedTransaction: SignedTransaction, peerId: PeerId): Boolean =
    knownTransactions.getOrElse(signedTransaction.hash, Set.empty).contains(peerId)

  private def setTxKnown(signedTransaction: SignedTransaction, peerId: PeerId): Unit = {
    val currentPeers = knownTransactions.getOrElse(signedTransaction.hash, Set.empty)
    val newPeers = currentPeers + peerId
    knownTransactions += (signedTransaction.hash -> newPeers)
  }

}
