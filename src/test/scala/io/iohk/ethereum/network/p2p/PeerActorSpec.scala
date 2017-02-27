package io.iohk.ethereum.network.p2p

import java.net.{InetSocketAddress, URI}

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.{Props, PoisonPill, Terminated, ActorSystem}
import akka.agent.Agent
import akka.testkit.{TestProbe, TestActorRef}
import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62.{GetBlockHeaders, BlockHeaders}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.utils.{Config, BlockchainStatus, ServerStatus, NodeStatus}
import org.spongycastle.util.encoders.Hex
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
    val peer = TestActorRef(Props(new PeerActor(nodeStatusHolder, _ => {
        rlpxConnection = TestProbe()
        rlpxConnection.ref
      })))

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

  it should "successfully connect to ETC peer" in new TestSetup with BlockUtils {
    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      Config.Blockchain.daoForkBlockTotalDifficulty + 10000000, // we're at some block, after the fork
      ByteString("unused"), 0)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = Config.Blockchain.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = Config.Blockchain.genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Pong()))
  }

  it should "disconnect from non-ETC peer" in new TestSetup with BlockUtils {
    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      Config.Blockchain.daoForkBlockTotalDifficulty + 10000000, // we're at some block, after the fork
      ByteString("unused"), 0)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = Config.Blockchain.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = Config.Blockchain.genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  it should "stay connected to non-ETC peer until reaching the fork" in new TestSetup with BlockUtils {
    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      Config.Blockchain.daoForkBlockTotalDifficulty - 10000000, // we're at some block, before the fork
      ByteString("unused"), 0)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = Config.Blockchain.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = Config.Blockchain.genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Pong) => () }

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  it should "stay connected to pre fork peer until reaching the fork" in new TestSetup with BlockUtils {
    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      Config.Blockchain.daoForkBlockTotalDifficulty - 10000000, // we're at some block, before the fork
      ByteString("unused"), 0)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = Config.Blockchain.daoForkBlockTotalDifficulty - 2000000, // remote is before the fork
      bestHash = ByteString("blockhash"),
      genesisHash = Config.Blockchain.genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Nil)))

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Pong) => () }

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  it should "disconnect on Hello timeout" in new TestSetup {
    val connection = TestProbe()

    peer ! PeerActor.HandleConnection(connection.ref, new InetSocketAddress("localhost", 9000))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.HandleConnection])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }

    rlpxConnection.expectMsg(5.seconds, RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.TimeoutOnReceivingAMessage)))
  }

  it should "update max peer when receiving new block" in new TestSetup {

  }

  it should "update max peer when receiving block header" in new TestSetup {

  }

  it should "update max peer when sending new block" in new TestSetup {

  }

  it should "update max peer when sending block header" in new TestSetup {

  }

  trait BlockUtils {

    val etcForkBlockHeader =
      BlockHeader(
        parentHash = ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("61c808d82a3ac53231750dadc13c777b59310bd9")),
        stateRoot = ByteString(Hex.decode("614d7d358b03cbdaf0343529673be20ad45809d02487f023e047efdce9da8aff")),
        transactionsRoot = ByteString(Hex.decode("d33068a7f21bff5018a00ca08a3566a06be4196dfe9e39f96e431565a619d455")),
        receiptsRoot = ByteString(Hex.decode("7bda9aa65977800376129148cbfe89d35a016dd51c95d6e6dc1e76307d315468")),
        logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
        difficulty = BigInt("62413376722602"),
        number = BigInt(1920000),
        gasLimit = BigInt(4712384),
        gasUsed = BigInt(84000),
        unixTimestamp = 1469020839L,
        extraData = ByteString(Hex.decode("e4b883e5bda9e7a59ee4bb99e9b1bc")),
        mixHash = ByteString(Hex.decode("c52daa7054babe515b17ee98540c0889cf5e1595c5dd77496997ca84a68c8da1")),
        nonce = ByteString(Hex.decode("05276a600980199d")))

    val nonEtcForkBlockHeader =
      BlockHeader(
        parentHash = ByteString("this"),
        ommersHash = ByteString("is"),
        beneficiary = ByteString("not"),
        stateRoot = ByteString("an"),
        transactionsRoot = ByteString("ETC"),
        receiptsRoot = ByteString("fork"),
        logsBloom = ByteString("block"),
        difficulty = BigInt("62413376722602"),
        number = BigInt(1920000),
        gasLimit = BigInt(4712384),
        gasUsed = BigInt(84000),
        unixTimestamp = 1469020839L,
        extraData = ByteString("unused"),
        mixHash = ByteString("unused"),
        nonce = ByteString("unused"))
  }

  trait NodeStatusSetup {
    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      blockchainStatus = BlockchainStatus(0, ByteString("123"), 0))

    val nodeStatusHolder = Agent(nodeStatus)
  }

  trait TestSetup extends NodeStatusSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")

    val rlpxConnection = TestProbe()

    val peer = TestActorRef(Props(new PeerActor(nodeStatusHolder, _ => rlpxConnection.ref)))
  }

}
