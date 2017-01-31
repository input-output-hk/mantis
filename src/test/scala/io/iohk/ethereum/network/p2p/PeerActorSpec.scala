package io.iohk.ethereum.network.p2p

import java.net.{InetSocketAddress, URI}

import akka.actor.{PoisonPill, Terminated, ActorSystem}
import akka.testkit.{TestProbe, TestActorRef}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.network.{NodeInfo, PeerActor}
import org.scalatest.{FlatSpec, Matchers}

class PeerActorSpec extends FlatSpec with Matchers {

  "PeerActor" should "create rlpx connection and send hello message" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    rlpxConnection.expectMsgPF() {
      case RLPxConnectionHandler.SendMessage(hello: Hello) => ()
    }
  }

  it should "retry failed rlpx connection" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.watch(peer)

    (0 to 3) foreach { _ =>
      rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
      rlpxConnection.reply(RLPxConnectionHandler.ConnectionFailed)
    }

    rlpxConnection.expectMsgClass(classOf[Terminated])
  }

  it should "try to reconnect on broken rlpx connection" in {
    implicit val system = ActorSystem("PeerActorSpec_System")
    val nodeInfo = NodeInfo(crypto.generateKeyPair(), new InetSocketAddress("127.0.0.1", 1))

    var rlpxConnection = TestProbe() // var as we actually need new instances
    val peer = TestActorRef(PeerActor.props(nodeInfo, _ => {
        rlpxConnection = TestProbe()
        rlpxConnection.ref
      }))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    rlpxConnection.expectMsgPF() {
      case RLPxConnectionHandler.SendMessage(hello: Hello) => ()
    }

    rlpxConnection.ref ! PoisonPill
    peer.unwatch(rlpxConnection.ref)
    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
  }

  trait TestSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")
    val nodeInfo = NodeInfo(crypto.generateKeyPair(), new InetSocketAddress("127.0.0.1", 1))

    val rlpxConnection = TestProbe()
    val peer = TestActorRef(PeerActor.props(nodeInfo, _ => rlpxConnection.ref))
  }

}
