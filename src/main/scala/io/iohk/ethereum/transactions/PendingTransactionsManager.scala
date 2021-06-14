package io.iohk.ethereum.transactions

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.{ByteString, Timeout}
import com.google.common.cache.{Cache, CacheBuilder, RemovalNotification}
import io.iohk.ethereum.domain.{SignedTransaction, SignedTransactionWithSender}
import io.iohk.ethereum.metrics.MetricsContainer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerEvent, Subscribe, SubscriptionClassifier}
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.SignedTransactions
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId, PeerManagerActor}
import io.iohk.ethereum.transactions.SignedTransactionsFilterActor.ProperSignedTransactions
import io.iohk.ethereum.utils.ByteStringUtils.ByteStringOps
import io.iohk.ethereum.utils.TxPoolConfig

import scala.jdk.CollectionConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object PendingTransactionsManager {
  def props(
      txPoolConfig: TxPoolConfig,
      peerManager: ActorRef,
      etcPeerManager: ActorRef,
      peerMessageBus: ActorRef
  ): Props =
    Props(new PendingTransactionsManager(txPoolConfig, peerManager, etcPeerManager, peerMessageBus))

  case class AddTransactions(signedTransactions: Set[SignedTransactionWithSender])

  case class AddUncheckedTransactions(signedTransactions: Seq[SignedTransaction])

  object AddTransactions {
    def apply(txs: SignedTransactionWithSender*): AddTransactions = AddTransactions(txs.toSet)
  }

  case class AddOrOverrideTransaction(signedTransaction: SignedTransaction)

  private case class NotifyPeers(signedTransactions: Seq[SignedTransactionWithSender], peers: Seq[Peer])

  case object GetPendingTransactions
  case class PendingTransactionsResponse(pendingTransactions: Seq[PendingTransaction])

  case class RemoveTransactions(signedTransactions: Seq[SignedTransaction])

  case class PendingTransaction(stx: SignedTransactionWithSender, addTimestamp: Long)

  case object ClearPendingTransactions
}

class PendingTransactionsManager(
    txPoolConfig: TxPoolConfig,
    peerManager: ActorRef,
    etcPeerManager: ActorRef,
    peerEventBus: ActorRef
) extends Actor
    with MetricsContainer
    with ActorLogging {

  import PendingTransactionsManager._
  import akka.pattern.ask

  private[this] final val TransactionsPoolSizeGauge =
    metrics.gauge(
      "transactions.pool.size.gauge",
      () => pendingTransactions.size().toDouble
    )

  /**
    * stores information which tx hashes are "known" by which peers
    */
  var knownTransactions: Map[ByteString, Set[PeerId]] = Map.empty

  /**
    * stores all pending transactions
    */
  val pendingTransactions: Cache[ByteString, PendingTransaction] = CacheBuilder
    .newBuilder()
    .expireAfterWrite(txPoolConfig.transactionTimeout._1, txPoolConfig.transactionTimeout._2)
    .maximumSize(txPoolConfig.txPoolSize)
    .removalListener((notification: RemovalNotification[ByteString, PendingTransaction]) =>
      if (notification.wasEvicted()) {
        log.debug("Evicting transaction: {} due to {}", notification.getKey.toHex, notification.getCause)
        knownTransactions = knownTransactions.filterNot(_._1 == notification.getKey)
      }
    )
    .build()

  implicit val timeout: Timeout = Timeout(3.seconds)

  peerEventBus ! Subscribe(SubscriptionClassifier.PeerHandshaked)

  val transactionFilter: ActorRef = context.actorOf(SignedTransactionsFilterActor.props(context.self, peerEventBus))

  // scalastyle:off method.length
  override def receive: Receive = {
    case PeerEvent.PeerHandshakeSuccessful(peer, _) =>
      pendingTransactions.cleanUp()
      val stxs = pendingTransactions.asMap().values().asScala.toSeq.map(_.stx)
      self ! NotifyPeers(stxs, Seq(peer))

    case AddUncheckedTransactions(transactions) =>
      val validTxs = SignedTransactionWithSender.getSignedTransactions(transactions)
      self ! AddTransactions(validTxs.toSet)

    case AddTransactions(signedTransactions) =>
      pendingTransactions.cleanUp()
      log.debug("Adding transactions: {}", signedTransactions.map(_.tx.hash.toHex))
      val stxs = pendingTransactions.asMap().values().asScala.map(_.stx).toSet
      val transactionsToAdd = signedTransactions.diff(stxs)
      if (transactionsToAdd.nonEmpty) {
        val timestamp = System.currentTimeMillis()
        transactionsToAdd.foreach(t => pendingTransactions.put(t.tx.hash, PendingTransaction(t, timestamp)))
        (peerManager ? PeerManagerActor.GetPeers)
          .mapTo[Peers]
          .map(_.handshaked)
          .filter(_.nonEmpty)
          .foreach { peers => self ! NotifyPeers(transactionsToAdd.toSeq, peers) }
      }

    case AddOrOverrideTransaction(newStx) =>
      pendingTransactions.cleanUp()
      log.debug("Overriding transaction: {}", newStx.hash.toHex)
      // Only validated transactions are added this way, it is safe to call get
      val newStxSender = SignedTransaction.getSender(newStx).get
      val obsoleteTxs = pendingTransactions
        .asMap()
        .asScala
        .filter(ptx => ptx._2.stx.senderAddress == newStxSender && ptx._2.stx.tx.tx.nonce == newStx.tx.nonce)
      pendingTransactions.invalidateAll(obsoleteTxs.keys.asJava)

      val timestamp = System.currentTimeMillis()
      val newPendingTx = SignedTransactionWithSender(newStx, newStxSender)
      pendingTransactions.put(newStx.hash, PendingTransaction(newPendingTx, timestamp))

      (peerManager ? PeerManagerActor.GetPeers)
        .mapTo[Peers]
        .map(_.handshaked)
        .filter(_.nonEmpty)
        .foreach { peers => self ! NotifyPeers(Seq(newPendingTx), peers) }

    case NotifyPeers(signedTransactions, peers) =>
      pendingTransactions.cleanUp()
      log.debug(
        "Notifying peers {} about transactions {}",
        peers.map(_.nodeId.map(_.toHex)),
        signedTransactions.map(_.tx.hash.toHex)
      )
      val pendingTxMap = pendingTransactions.asMap()
      val stillPending = signedTransactions
        .filter(stx => pendingTxMap.containsKey(stx.tx.hash)) // signed transactions that are still pending

      peers.foreach { peer =>
        val txsToNotify = stillPending.filterNot(stx => isTxKnown(stx.tx, peer.id)) // and not known by peer
        if (txsToNotify.nonEmpty) {
          etcPeerManager ! EtcPeerManagerActor.SendMessage(SignedTransactions(txsToNotify.map(_.tx)), peer.id)
          txsToNotify.foreach(stx => setTxKnown(stx.tx, peer.id))
        }
      }

    case GetPendingTransactions =>
      pendingTransactions.cleanUp()
      sender() ! PendingTransactionsResponse(pendingTransactions.asMap().asScala.values.toSeq)

    case RemoveTransactions(signedTransactions) =>
      pendingTransactions.invalidateAll(signedTransactions.map(_.hash).asJava)
      log.debug("Removing transactions: {}", signedTransactions.map(_.hash.toHex))
      knownTransactions = knownTransactions -- signedTransactions.map(_.hash)

    case ProperSignedTransactions(transactions, peerId) =>
      self ! AddTransactions(transactions)
      transactions.foreach(stx => setTxKnown(stx.tx, peerId))

    case ClearPendingTransactions =>
      log.debug("Dropping all cached transactions")
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
