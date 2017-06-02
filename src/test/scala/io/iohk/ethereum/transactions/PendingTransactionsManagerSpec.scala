package io.iohk.ethereum.transactions

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.{DefaultPatience, crypto}
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import io.iohk.ethereum.network.{Network, PeerId, PeerImpl}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.transactions.PendingTransactionsManager._
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import akka.pattern.ask
import io.iohk.ethereum.network.PeerActor.SendMessage
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.p2p.messages.CommonMessages
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future
import scala.concurrent.duration._

class PendingTransactionsManagerSpec extends FlatSpec with Matchers with ScalaFutures with DefaultPatience {

  implicit val timeout = Timeout(10.seconds)

  "PendingTransactionsManager" should "store pending transactions received from peers" in new TestSetup {
    val msg = SignedTransactions(Seq.fill(10)(newStx()))

    (network.peers _).expects().returning(
      Future.successful(Peers(Map(peer1 -> handshakedStatus, peer2 -> handshakedStatus, peer3 -> handshakedStatus)))
    )
    pendingTransactionsManager ! MessageFromPeer(msg, PeerId("1"))

    val pendingTxs = (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactions].futureValue
    pendingTxs.signedTransactions.toSet shouldBe msg.txs.toSet
  }

  it should "broadcast received pending transactions to other peers" in new TestSetup {
    val stx = newStx()

    (network.peers _).expects().returning(
      Future.successful(Peers(Map(peer1 -> handshakedStatus, peer2 -> handshakedStatus, peer3 -> handshakedStatus)))
    )
    pendingTransactionsManager ! AddTransaction(stx)

    Seq(peer1TestProbe, peer2TestProbe, peer3TestProbe).foreach { p =>
      p.expectMsg(SendMessage(SignedTransactions(Seq(stx))))
    }

    val pendingTxs = (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactions].futureValue
    pendingTxs.signedTransactions shouldBe Seq(stx)
  }

  it should "notify other peers about received transactions and handle removal" in new TestSetup {
    val msg1 = SignedTransactions(Seq.fill(10)(newStx()))

    (network.peers _).expects().returning(
      Future.successful(Peers(Map(peer1 -> handshakedStatus, peer2 -> handshakedStatus, peer3 -> handshakedStatus)))
    )
    pendingTransactionsManager ! MessageFromPeer(msg1, peer1.id)

    Seq(peer2TestProbe, peer3TestProbe).foreach { p =>
      p.expectMsg(SendMessage(SignedTransactions(msg1.txs)))
    }
    peer1TestProbe.expectNoMsg()

    val msg2 = SignedTransactions(Seq.fill(5)(newStx()))

    (network.peers _).expects().returning(
      Future.successful(Peers(Map(peer1 -> handshakedStatus, peer2 -> handshakedStatus, peer3 -> handshakedStatus)))
    )
    pendingTransactionsManager ! MessageFromPeer(msg2, peer2.id)

    Seq(peer1TestProbe, peer3TestProbe).foreach { p =>
      p.expectMsg(SendMessage(SignedTransactions(msg2.txs)))
    }
    peer2TestProbe.expectNoMsg()

    pendingTransactionsManager ! RemoveTransactions(msg1.txs.dropRight(4))
    pendingTransactionsManager ! RemoveTransactions(msg2.txs.drop(2))

    val pendingTxs = (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactions].futureValue
    pendingTxs.signedTransactions.size shouldBe 6
    pendingTxs.signedTransactions shouldBe msg1.txs.takeRight(4) ++ msg2.txs.take(2)
  }

  it should "not add pending transaction again when it was removed while waiting for peers" in new TestSetup {
    val msg1 = SignedTransactions(Seq.fill(1)(newStx()))

    (network.peers _).expects().returning(
      Future.successful(Peers(Map(peer1 -> handshakedStatus, peer2 -> handshakedStatus, peer3 -> handshakedStatus)))
    )
    pendingTransactionsManager ! MessageFromPeer(msg1, peer1.id)

    pendingTransactionsManager ! RemoveTransactions(msg1.txs)

    Seq(peer1TestProbe, peer2TestProbe, peer3TestProbe).foreach { peer =>
      peer.expectNoMsg()
    }

    val pendingTxs = (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactions].futureValue
    pendingTxs.signedTransactions.size shouldBe 0
  }

  trait TestSetup extends MockFactory {
    implicit val system = ActorSystem("test-system")

    def newStx(): SignedTransaction = {
      val keyPair1 = crypto.generateKeyPair()
      val addr1 = Address(Hex.decode("1c51bf013add0857c5d9cf2f71a7f15ca93d4816"))
      val tx = Transaction(0, 1, 1, Some(addr1), 0, ByteString(""))
      SignedTransaction.sign(tx, keyPair1, Some(0x3d))
    }

    val handshakedStatus = Handshaked(CommonMessages.Status(0, 0, 0, ByteString(""), ByteString("")), true, 0)

    val network = mock[Network]
    (network.subscribe(_: Set[Int])(_: ActorRef)).expects(Set(SignedTransactions.code), *).returns(())
    val pendingTransactionsManager = system.actorOf(
      PendingTransactionsManager.props(network)
    )

    val peerMessageBus = TestProbe()
    val peer1TestProbe = TestProbe()
    val peer1 = PeerImpl(new InetSocketAddress("127.0.0.1", 9000), peer1TestProbe.ref, peerMessageBus.ref)
    val peer2TestProbe = TestProbe()
    val peer2 = PeerImpl(new InetSocketAddress("127.0.0.2", 9000), peer2TestProbe.ref, peerMessageBus.ref)
    val peer3TestProbe = TestProbe()
    val peer3 = PeerImpl(new InetSocketAddress("127.0.0.3", 9000), peer3TestProbe.ref, peerMessageBus.ref)
  }

}
