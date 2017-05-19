package io.iohk.ethereum.transactions

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import io.iohk.ethereum.network.{PeerId, PeerManagerActor}
import io.iohk.ethereum.network.PeerMessageBusActor.MessageFromPeer
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.transactions.PendingTransactionsManager.{BroadcastTransaction, GetPendingTransactions, PendingTransactions, RemoveTransactions}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import akka.pattern.ask
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._

class PendingTransactionsManagerSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val timeout = Timeout(2.seconds)

  "PendingTransactionsManager" should "store pending transactions received from peers" in new TestSetup {
    val msg = SignedTransactions(Seq.fill(10)(newStx()))
    pendingTransactionsManager ! MessageFromPeer(msg, PeerId("1"))

    val pendingTxs = (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactions].futureValue
    pendingTxs.signedTransactions shouldBe msg.txs
  }

  it should "broadcast received pending transactions and store them" in new TestSetup {
    val stx = newStx()
    pendingTransactionsManager ! BroadcastTransaction(stx)

    val pendingTxs = (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactions].futureValue
    pendingTxs.signedTransactions shouldBe Seq(stx)

    peerManager.expectMsg(PeerManagerActor.BroadcastMessage(SignedTransactions(Seq(stx))))
  }

  it should "remove pending transactions from the list" in new TestSetup {
    val msg1 = SignedTransactions(Seq.fill(10)(newStx()))
    val msg2 = SignedTransactions(Seq.fill(5)(newStx()))
    pendingTransactionsManager ! MessageFromPeer(msg1, PeerId("1"))
    pendingTransactionsManager ! MessageFromPeer(msg2, PeerId("99"))

    pendingTransactionsManager ! RemoveTransactions(msg1.txs.dropRight(4))
    pendingTransactionsManager ! RemoveTransactions(msg2.txs.drop(2))

    val pendingTxs = (pendingTransactionsManager ? GetPendingTransactions).mapTo[PendingTransactions].futureValue
    pendingTxs.signedTransactions.size shouldBe 6
    pendingTxs.signedTransactions shouldBe msg1.txs.takeRight(4) ++ msg2.txs.take(2)
  }

  trait TestSetup {

    def newStx(): SignedTransaction = {
      val keyPair1 = crypto.generateKeyPair()
      val addr1 = Address(Hex.decode("1c51bf013add0857c5d9cf2f71a7f15ca93d4816"))
      val tx = Transaction(0, 1, 1, Some(addr1), 0, ByteString(""))
      SignedTransaction.sign(tx, keyPair1, 0x3d)
    }

    implicit val system = ActorSystem("test-system")
    val peerManager = TestProbe()
    val peerMessageBus = TestProbe()
    val pendingTransactionsManager = system.actorOf(PendingTransactionsManager.props(peerManager.ref, peerMessageBus.ref))
  }

}
