package io.iohk.ethereum.transactions

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, SignedTransaction, SignedTransactionWithSender, Transaction}
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeResult
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId, PeerManagerActor}
import io.iohk.ethereum.transactions.PendingTransactionsManager._
import io.iohk.ethereum.{NormalPatience, Timeouts, crypto}
import io.iohk.ethereum.transactions.SignedTransactionsFilterActor.ProperSignedTransactions
import io.iohk.ethereum.utils.TxPoolConfig
import org.scalatest.concurrent.ScalaFutures
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

import scala.concurrent.duration._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PendingTransactionsManagerSpec extends AnyFlatSpec with Matchers with ScalaFutures with NormalPatience {

  "PendingTransactionsManager" should "store pending transactions received from peers" in new TestSetup {
    val msg = ((1 to 10).map(e => newStx(e))).toSet
    pendingTransactionsManager ! ProperSignedTransactions(msg, PeerId("1"))

    Thread.sleep(Timeouts.normalTimeout.toMillis)

    val pendingTxs =
      (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactionsResponse].futureValue
    pendingTxs.pendingTransactions.map(_.stx).toSet shouldBe msg
  }

  it should "ignore known transaction" in new TestSetup {
    val msg = Seq(newStx(1)).toSet
    pendingTransactionsManager ! ProperSignedTransactions(msg, PeerId("1"))
    pendingTransactionsManager ! ProperSignedTransactions(msg, PeerId("2"))

    Thread.sleep(Timeouts.normalTimeout.toMillis)

    val pendingTxs =
      (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactionsResponse].futureValue
    pendingTxs.pendingTransactions.map(_.stx).length shouldBe 1
    pendingTxs.pendingTransactions.map(_.stx).toSet shouldBe msg
  }

  it should "broadcast received pending transactions to other peers" in new TestSetup {
    val stx = newStx()
    pendingTransactionsManager ! AddTransactions(stx)

    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map(peer1 -> Handshaked, peer2 -> Handshaked, peer3 -> Handshaked)))

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(SignedTransactions(Seq(stx.tx)), peer1.id),
      EtcPeerManagerActor.SendMessage(SignedTransactions(Seq(stx.tx)), peer2.id),
      EtcPeerManagerActor.SendMessage(SignedTransactions(Seq(stx.tx)), peer3.id)
    )

    val pendingTxs =
      (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactionsResponse].futureValue
    pendingTxs.pendingTransactions.map(_.stx) shouldBe Seq(stx)
  }

  it should "notify other peers about received transactions and handle removal" in new TestSetup {
    val tx1 = Seq.fill(10)(newStx())
    val msg1 = tx1.toSet
    pendingTransactionsManager ! ProperSignedTransactions(msg1, peer1.id)
    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map(peer1 -> Handshaked, peer2 -> Handshaked, peer3 -> Handshaked)))

    val resps1 = etcPeerManager.expectMsgAllConformingOf(
      classOf[EtcPeerManagerActor.SendMessage],
      classOf[EtcPeerManagerActor.SendMessage]
    )

    resps1.map(_.peerId) should contain.allOf(peer2.id, peer3.id)
    resps1.map(_.message.underlyingMsg).foreach { case SignedTransactions(txs) => txs.toSet shouldEqual msg1.map(_.tx) }
    etcPeerManager.expectNoMessage()

    val tx2 = Seq.fill(5)(newStx())
    val msg2 = tx2.toSet
    pendingTransactionsManager ! ProperSignedTransactions(msg2, peer2.id)
    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map(peer1 -> Handshaked, peer2 -> Handshaked, peer3 -> Handshaked)))

    val resps2 = etcPeerManager.expectMsgAllConformingOf(
      classOf[EtcPeerManagerActor.SendMessage],
      classOf[EtcPeerManagerActor.SendMessage]
    )
    resps2.map(_.peerId) should contain.allOf(peer1.id, peer3.id)
    resps2.map(_.message.underlyingMsg).foreach { case SignedTransactions(txs) => txs.toSet shouldEqual msg2.map(_.tx) }
    etcPeerManager.expectNoMessage()

    pendingTransactionsManager ! RemoveTransactions(tx1.dropRight(4).map(_.tx))
    pendingTransactionsManager ! RemoveTransactions(tx2.drop(2).map(_.tx))

    val pendingTxs =
      (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactionsResponse].futureValue
    pendingTxs.pendingTransactions.size shouldBe 6
    pendingTxs.pendingTransactions.map(_.stx).toSet shouldBe (tx2.take(2) ++ tx1.takeRight(4)).toSet
  }

  it should "not add pending transaction again when it was removed while waiting for peers" in new TestSetup {
    val msg1 = Set(newStx(1))
    pendingTransactionsManager ! ProperSignedTransactions(msg1, peer1.id)
    Thread.sleep(Timeouts.normalTimeout.toMillis)
    pendingTransactionsManager ! RemoveTransactions(msg1.map(_.tx).toSeq)

    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map(peer1 -> Handshaked, peer2 -> Handshaked, peer3 -> Handshaked)))

    etcPeerManager.expectNoMessage()

    val pendingTxs =
      (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactionsResponse].futureValue
    pendingTxs.pendingTransactions.size shouldBe 0
  }

  it should "override transactions with the same sender and nonce" in new TestSetup {
    val firstTx = newStx(1, tx, keyPair1)
    val otherTx = newStx(1, tx, keyPair2)
    val overrideTx = newStx(1, tx.copy(value = 2 * tx.value), keyPair1)

    pendingTransactionsManager ! AddOrOverrideTransaction(firstTx.tx)
    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map(peer1 -> Handshaked)))
    Thread.sleep(Timeouts.shortTimeout.toMillis)

    pendingTransactionsManager ! AddOrOverrideTransaction(otherTx.tx)
    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map(peer1 -> Handshaked)))
    Thread.sleep(Timeouts.shortTimeout.toMillis)

    pendingTransactionsManager ! AddOrOverrideTransaction(overrideTx.tx)
    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map(peer1 -> Handshaked)))
    Thread.sleep(Timeouts.shortTimeout.toMillis)

    val pendingTxs = (pendingTransactionsManager ? GetPendingTransactions)
      .mapTo[PendingTransactionsResponse]
      .futureValue
      .pendingTransactions

    pendingTxs.map(_.stx).toSet shouldEqual Set(overrideTx, otherTx)

    // overriden TX will still be broadcast to peers
    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(SignedTransactions(List(firstTx.tx)), peer1.id),
      EtcPeerManagerActor.SendMessage(SignedTransactions(List(otherTx.tx)), peer1.id),
      EtcPeerManagerActor.SendMessage(SignedTransactions(List(overrideTx.tx)), peer1.id)
    )
  }

  it should "broadcast pending transactions to newly connected peers" in new TestSetup {
    val stx = newStx()
    pendingTransactionsManager ! AddTransactions(stx)

    peerManager.expectMsg(PeerManagerActor.GetPeers)
    peerManager.reply(Peers(Map.empty))

    pendingTransactionsManager ! PeerEvent.PeerHandshakeSuccessful(peer1, new HandshakeResult {})

    etcPeerManager.expectMsgAllOf(EtcPeerManagerActor.SendMessage(SignedTransactions(Seq(stx.tx)), peer1.id))
  }

  it should "remove transaction on timeout" in new TestSetup {
    override val txPoolConfig = new TxPoolConfig {
      override val txPoolSize: Int = 300
      override val transactionTimeout: FiniteDuration = 500.millis
      override val getTransactionFromPoolTimeout: FiniteDuration = Timeouts.normalTimeout

      //unused
      override val pendingTxManagerQueryTimeout: FiniteDuration = Timeouts.veryLongTimeout
    }

    override val pendingTransactionsManager = system.actorOf(
      PendingTransactionsManager.props(txPoolConfig, peerManager.ref, etcPeerManager.ref, peerMessageBus.ref)
    )

    val stx = newStx()
    pendingTransactionsManager ! AddTransactions(stx)

    val pendingTxs =
      (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactionsResponse].futureValue
    pendingTxs.pendingTransactions.map(_.stx).toSet shouldBe Set(stx)

    Thread.sleep(550)

    val pendingTxsAfter =
      (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactionsResponse].futureValue
    pendingTxsAfter.pendingTransactions.map(_.stx).toSet shouldBe Set.empty
  }

  trait TestSetup extends SecureRandomBuilder {
    implicit val system = ActorSystem("test-system")

    val keyPair1 = crypto.generateKeyPair(secureRandom)
    val keyPair2 = crypto.generateKeyPair(secureRandom)

    val tx = Transaction(1, 1, 1, Some(Address(42)), 10, ByteString(""))

    def newStx(
        nonce: BigInt = 0,
        tx: Transaction = tx,
        keyPair: AsymmetricCipherKeyPair = crypto.generateKeyPair(secureRandom)
    ): SignedTransactionWithSender =
      SignedTransaction.sign(tx, keyPair, Some(0x3d))

    val peer1TestProbe = TestProbe()
    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 9000), peer1TestProbe.ref, false)
    val peer2TestProbe = TestProbe()
    val peer2 = Peer(new InetSocketAddress("127.0.0.2", 9000), peer2TestProbe.ref, false)
    val peer3TestProbe = TestProbe()
    val peer3 = Peer(new InetSocketAddress("127.0.0.3", 9000), peer3TestProbe.ref, false)

    val txPoolConfig = new TxPoolConfig {
      override val txPoolSize: Int = 300

      //unused
      override val pendingTxManagerQueryTimeout: FiniteDuration = Timeouts.veryLongTimeout
      override val transactionTimeout: FiniteDuration = Timeouts.veryLongTimeout
      override val getTransactionFromPoolTimeout: FiniteDuration = Timeouts.veryLongTimeout
    }

    val peerManager = TestProbe()
    val etcPeerManager = TestProbe()
    val peerMessageBus = TestProbe()
    val pendingTransactionsManager = system.actorOf(
      PendingTransactionsManager.props(txPoolConfig, peerManager.ref, etcPeerManager.ref, peerMessageBus.ref)
    )
  }

}
