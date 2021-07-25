package io.iohk.ethereum.network

import java.net.InetSocketAddress
import java.net.URI

import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.TestActorRef
import akka.testkit.TestProbe
import akka.util.ByteString

import com.miguno.akka.testing.VirtualTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.Mocks.MockHandshakerAlwaysFails
import io.iohk.ethereum.Mocks.MockHandshakerAlwaysSucceeds
import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.PeerActor.ConnectTo
import io.iohk.ethereum.network.PeerActor.GetStatus
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerActor.StatusResponse
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.handshaker._
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.Status
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Pong
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.utils.Config

class PeerActorHandshakingSpec extends AnyFlatSpec with Matchers {

  it should "succeed in establishing connection if the handshake is always successful" in new TestSetup {

    import DefaultValues._

    val peerActorHandshakeSucceeds =
      peerActor(MockHandshakerAlwaysSucceeds(defaultStatus, defaultBlockNumber, defaultForkAccepted))

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
    peerActorHandshakeRequiresHello ! RLPxConnectionHandler.MessageReceived(defaultStatusMsg)

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

  trait TestSetup extends EphemBlockchainTestSetup {
    implicit override lazy val system: ActorSystem = ActorSystem("PeerActorSpec_System")

    val time = new VirtualTime

    val uri = new URI(
      "enode://18a551bee469c2e02de660ab01dede06503c986f6b8520cb5a65ad122df88b17b285e3fef09a40a0d44f99e014f8616cf1ebc2e094f96c6e09e2f390f5d34857@47.90.36.129:30303"
    )
    val rlpxConnectionProbe: TestProbe = TestProbe()
    val peerMessageBus: TestProbe = TestProbe()
    val knownNodesManager: TestProbe = TestProbe()

    def peerActor(handshaker: Handshaker[PeerInfo]): TestActorRef[PeerActor[PeerInfo]] = TestActorRef(
      Props(
        new PeerActor(
          new InetSocketAddress("127.0.0.1", 0),
          rlpxConnectionFactory = _ => rlpxConnectionProbe.ref,
          peerConfiguration = Config.Network.peer,
          peerEventBus = peerMessageBus.ref,
          knownNodesManager = knownNodesManager.ref,
          incomingConnection = false,
          externalSchedulerOpt = Some(time.scheduler),
          initHandshaker = handshaker
        )
      )
    )
  }

  object DefaultValues {
    val defaultStatusMsg: Status = Status(
      protocolVersion = Capability.ETH63.version,
      networkId = 1,
      totalDifficulty = Fixtures.Blocks.Genesis.header.difficulty,
      bestHash = Fixtures.Blocks.Genesis.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val defaultStatus: RemoteStatus = RemoteStatus(defaultStatusMsg)
    val defaultBlockNumber = 1000
    val defaultForkAccepted = true

    val defaultPeerInfo: PeerInfo = PeerInfo(
      defaultStatus,
      defaultStatus.chainWeight,
      defaultForkAccepted,
      defaultBlockNumber,
      defaultStatus.bestHash
    )

    val defaultReasonDisconnect = Disconnect.Reasons.Other

    val defaultHello: Hello = Hello(
      p2pVersion = 0,
      clientId = "notused",
      capabilities = Seq(Capability.ETH63),
      listenPort = 0,
      nodeId = ByteString.empty
    )
    val defaultTimeout = Timeouts.normalTimeout
  }

  case class MockHandshakerRequiresHello private (handshakerState: HandshakerState[PeerInfo])
      extends Handshaker[PeerInfo] {
    override def copy(newState: HandshakerState[PeerInfo]): Handshaker[PeerInfo] = new MockHandshakerRequiresHello(
      newState
    )
  }

  object MockHandshakerRequiresHello {
    def apply(): MockHandshakerRequiresHello =
      new MockHandshakerRequiresHello(MockHelloExchangeState)
  }

  case object MockHelloExchangeState extends InProgressState[PeerInfo] {

    import DefaultValues._

    def nextMessage: NextMessage = NextMessage(defaultHello, defaultTimeout)

    def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = {
      case helloMsg: Hello => ConnectedState(defaultPeerInfo)
      case status: Status  => DisconnectedState(defaultReasonDisconnect)
    }

    def processTimeout: HandshakerState[PeerInfo] = DisconnectedState(defaultReasonDisconnect)
  }

}
