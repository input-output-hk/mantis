package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.{Fixtures, Timeouts}
import io.iohk.ethereum.Mocks.{MockHandshakerAlwaysFails, MockHandshakerAlwaysSucceeds}
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerActor.{ConnectTo, GetStatus, StatusResponse}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.handshaker._
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Disconnect, Hello, Pong}
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.utils.Config
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class PeerActorHandshakingSpec extends FlatSpec with Matchers {

  it should "succeed in establishing connection if the handshake is always successful" in new TestSetup {

    import DefaultValues._

    val peerActorHandshakeSucceeds = peerActor(MockHandshakerAlwaysSucceeds(defaultStatus, defaultBlockNumber, defaultForkAccepted))

    //Establish probe rlpxconnection
    peerActorHandshakeSucceeds ! ConnectTo(uri)
    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.ConnectTo(uri))
    rlpxConnectionProbe.reply(RLPxConnectionHandler.ConnectionEstablished(ByteString()))

    //Test that the handshake succeeded
    val sender = TestProbe()(system)
    sender.send(peerActorHandshakeSucceeds, GetStatus)
    sender.expectMsg(StatusResponse(Handshaked))
  }

  it should "fail in establishing connection if the handshake always fails" in new TestSetup {

    import DefaultValues._

    val peerActorHandshakeFails = peerActor(MockHandshakerAlwaysFails(defaultReasonDisconnect))

    //Establish probe rlpxconnection
    peerActorHandshakeFails ! ConnectTo(uri)
    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.ConnectTo(uri))
    rlpxConnectionProbe.reply(RLPxConnectionHandler.ConnectionEstablished(ByteString()))

    //Test that the handshake failed
    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(defaultReasonDisconnect)))

  }

  it should "succeed in establishing connection in simple Hello exchange" in new TestSetup {

    import DefaultValues._

    val peerActorHandshakeRequiresHello = peerActor(MockHandshakerRequiresHello())

    //Establish probe rlpxconnection
    peerActorHandshakeRequiresHello ! ConnectTo(uri)
    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.ConnectTo(uri))
    rlpxConnectionProbe.reply(RLPxConnectionHandler.ConnectionEstablished(ByteString()))

    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.SendMessage(defaultHello))
    peerActorHandshakeRequiresHello ! RLPxConnectionHandler.MessageReceived(defaultHello)

    //Test that the handshake succeeded
    val sender = TestProbe()(system)
    sender.send(peerActorHandshakeRequiresHello, GetStatus)
    sender.expectMsg(StatusResponse(Handshaked))
  }

  it should "fail in establishing connection in simple Hello exchange if timeout happened" in new TestSetup {

    import DefaultValues._

    val peerActorHandshakeRequiresHello = peerActor(MockHandshakerRequiresHello())

    //Establish probe rlpxconnection
    peerActorHandshakeRequiresHello ! ConnectTo(uri)
    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.ConnectTo(uri))
    rlpxConnectionProbe.reply(RLPxConnectionHandler.ConnectionEstablished(ByteString()))

    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.SendMessage(defaultHello))
    time.advance(defaultTimeout * 2)

    //Test that the handshake failed
    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(defaultReasonDisconnect)))
  }

  it should "fail in establishing connection in simple Hello exchange if a Status message was received" in new TestSetup {

    import DefaultValues._

    val peerActorHandshakeRequiresHello = peerActor(MockHandshakerRequiresHello())

    //Establish probe rlpxconnection
    peerActorHandshakeRequiresHello ! ConnectTo(uri)
    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.ConnectTo(uri))
    rlpxConnectionProbe.reply(RLPxConnectionHandler.ConnectionEstablished(ByteString()))

    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.SendMessage(defaultHello))
    peerActorHandshakeRequiresHello ! RLPxConnectionHandler.MessageReceived(defaultStatus)

    //Test that the handshake failed
    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(defaultReasonDisconnect)))
  }

  it should "ignore unhandled message while establishing connection" in new TestSetup {

    import DefaultValues._

    val peerActorHandshakeRequiresHello = peerActor(MockHandshakerRequiresHello())

    //Establish probe rlpxconnection
    peerActorHandshakeRequiresHello ! ConnectTo(uri)
    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.ConnectTo(uri))
    rlpxConnectionProbe.reply(RLPxConnectionHandler.ConnectionEstablished(ByteString()))

    rlpxConnectionProbe.expectMsg(RLPxConnectionHandler.SendMessage(defaultHello))
    peerActorHandshakeRequiresHello ! RLPxConnectionHandler.MessageReceived(Pong()) //Ignored
    peerActorHandshakeRequiresHello ! RLPxConnectionHandler.MessageReceived(Pong()) //Ignored
    peerActorHandshakeRequiresHello ! RLPxConnectionHandler.MessageReceived(Pong()) //Ignored
    peerActorHandshakeRequiresHello ! RLPxConnectionHandler.MessageReceived(defaultHello)

    //Test that the handshake succeeded
    val sender = TestProbe()(system)
    sender.send(peerActorHandshakeRequiresHello, GetStatus)
    sender.expectMsg(StatusResponse(Handshaked))
  }

  trait TestSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")

    val time = new VirtualTime

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain = BlockchainImpl(storagesInstance.storages)

    val uri = new URI("enode://18a551bee469c2e02de660ab01dede06503c986f6b8520cb5a65ad122df88b17b285e3fef09a40a0d44f99e014f8616cf1ebc2e094f96c6e09e2f390f5d34857@47.90.36.129:30303")
    val rlpxConnectionProbe = TestProbe()
    val peerMessageBus = TestProbe()
    val knownNodesManager = TestProbe()

    def peerActor(handshaker: Handshaker[PeerInfo]): TestActorRef[PeerActor[PeerInfo]] = TestActorRef(Props(new PeerActor(
      new InetSocketAddress("127.0.0.1", 0),
      rlpxConnectionFactory = _ => rlpxConnectionProbe.ref,
      peerConfiguration = Config.Network.peer,
      peerEventBus = peerMessageBus.ref,
      knownNodesManager = knownNodesManager.ref,
      externalSchedulerOpt = Some(time.scheduler),
      initHandshaker = handshaker
    )))
  }

  object DefaultValues {
    val defaultStatus = Status(
      protocolVersion = Versions.PV63,
      networkId = 1,
      totalDifficulty = Fixtures.Blocks.Genesis.header.difficulty,
      bestHash = Fixtures.Blocks.Genesis.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val defaultBlockNumber = 1000
    val defaultForkAccepted = true

    val defaultPeerInfo = PeerInfo(defaultStatus, defaultStatus.totalDifficulty, defaultForkAccepted, defaultBlockNumber)

    val defaultReasonDisconnect = Disconnect.Reasons.Other

    val defaultHello = Hello(
      p2pVersion = 0,
      clientId = "notused",
      capabilities = Nil,
      listenPort = 0,
      nodeId = ByteString.empty
    )
    val defaultTimeout = Timeouts.normalTimeout
  }

  case class MockHandshakerRequiresHello private (handshakerState: HandshakerState[PeerInfo]) extends Handshaker[PeerInfo] {
    override def copy(newState: HandshakerState[PeerInfo]): Handshaker[PeerInfo] = new MockHandshakerRequiresHello(newState)
  }

  object MockHandshakerRequiresHello {
    def apply(): MockHandshakerRequiresHello = {
      new MockHandshakerRequiresHello(MockHelloExchangeState)
    }
  }

  case object MockHelloExchangeState extends InProgressState[PeerInfo] {

    import DefaultValues._

    def nextMessage: NextMessage = NextMessage(defaultHello, defaultTimeout)

    def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = {
      case helloMsg: Hello => ConnectedState(defaultPeerInfo)
      case status: Status => DisconnectedState(defaultReasonDisconnect)
    }

    def processTimeout: HandshakerState[PeerInfo] = DisconnectedState(defaultReasonDisconnect)
  }

}
