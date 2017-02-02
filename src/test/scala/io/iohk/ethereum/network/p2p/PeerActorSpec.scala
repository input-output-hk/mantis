package io.iohk.ethereum.network.p2p

import java.net.URI

import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.{PoisonPill, Terminated, ActorSystem}
import akka.agent.Agent
import akka.testkit.{TestProbe, TestActorRef}
import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.utils.{BlockchainStatus, ServerStatus, NodeStatus}
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

  it should "try to reconnect on broken rlpx connection" in new NodeStatusSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")

    var rlpxConnection = TestProbe() // var as we actually need new instances
    val peer = TestActorRef(PeerActor.props(nodeStatusHolder, _ => {
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

  trait NodeStatusSetup {
    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      blockchainStatus = BlockchainStatus(0, ByteString("123")))

    val nodeStatusHolder = Agent(nodeStatus)
  }

  trait TestSetup extends NodeStatusSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")

    val rlpxConnection = TestProbe()

    val peer = TestActorRef(PeerActor.props(nodeStatusHolder, _ => rlpxConnection.ref))
  }

}
