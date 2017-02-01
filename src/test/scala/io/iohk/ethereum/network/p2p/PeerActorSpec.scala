package io.iohk.ethereum.network.p2p

import java.net.URI

import akka.actor.{PoisonPill, Terminated, ActorSystem}
import akka.testkit.{TestProbe, TestActorRef}
import akka.util.ByteString
import io.iohk.ethereum.NodeStatusHolder.{BlockchainStatus, ServerStatus, NodeStatus, NodeStatusResponse}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Disconnect, Hello}
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.network.PeerActor
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class PeerActorSpec extends FlatSpec with Matchers {

  "PeerActor" should "create rlpx connection and send hello message" in new TestSetup with NodeStatusSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    nodeStatusHolder.receiveOne(5.seconds)
    nodeStatusHolder.reply(NodeStatusResponse(nodeStatus))

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

  it should "try to reconnect on broken rlpx connection" in new NodeStatusSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")

    val nodeStatusHolder = TestProbe()
    var rlpxConnection = TestProbe() // var as we actually need new instances
    val peer = TestActorRef(PeerActor.props(nodeStatusHolder.ref, _ => {
        rlpxConnection = TestProbe()
        rlpxConnection.ref
      }))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    nodeStatusHolder.receiveOne(5.seconds)
    nodeStatusHolder.reply(NodeStatusResponse(nodeStatus))

    rlpxConnection.expectMsgPF() {
      case RLPxConnectionHandler.SendMessage(hello: Hello) => ()
    }

    rlpxConnection.ref ! PoisonPill
    peer.unwatch(rlpxConnection.ref)
    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
  }

  it should "stash incoming messages until received node status and sent Hello" in new TestSetup with NodeStatusSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    peer ! RLPxConnectionHandler.MessageReceived(Hello(1, "test-client", Nil, 0, ByteString("")))

    nodeStatusHolder.receiveOne(5.seconds)
    nodeStatusHolder.reply(NodeStatusResponse(nodeStatus))

    rlpxConnection.expectMsgPF() {
      case RLPxConnectionHandler.SendMessage(hello: Hello) => ()
    }

    rlpxConnection.expectMsgPF() {
      case RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.IncompatibleP2pProtocolVersion)) => ()
    }
  }

  trait NodeStatusSetup {
    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      blockchainStatus = BlockchainStatus(0, ByteString("123")))
  }

  trait TestSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")

    val nodeStatusHolder = TestProbe()
    val rlpxConnection = TestProbe()

    val peer = TestActorRef(PeerActor.props(nodeStatusHolder.ref, _ => rlpxConnection.ref))
  }

}
