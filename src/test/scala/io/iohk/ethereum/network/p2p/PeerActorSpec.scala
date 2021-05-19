package io.iohk.ethereum.network.p2p

import java.net.{InetSocketAddress, URI}
import java.security.SecureRandom
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorSystem, PoisonPill, Props, Terminated}
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum._
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerActor.{GetStatus, StatusResponse}
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration}
import io.iohk.ethereum.network.p2p.Message.Version
import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status.StatusEnc
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockHeaders.GetBlockHeadersEnc
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.{PV164, ProtocolVersions}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect.Reasons
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello.HelloEnc
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Pong.PongEnc
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.network.{ForkResolver, PeerActor, PeerEventBusActor, _}
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.language.postfixOps

class PeerActorSpec
    extends TestKit(ActorSystem("PeerActorSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers {

  val remoteNodeKey: AsymmetricCipherKeyPair = generateKeyPair(new SecureRandom)
  val remoteNodeId: ByteString = ByteString(remoteNodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId)

  val blockchainConfig = Config.blockchains.blockchainConfig

  "PeerActor" should "create rlpx connection and send hello message" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(hello: HelloEnc) =>
      ()
    }
  }

  it should "retry failed rlpx connection" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.watch(peer)

    (0 to 3) foreach { _ =>
      time.advance(5.seconds)
      rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
      rlpxConnection.reply(RLPxConnectionHandler.ConnectionFailed)
    }

    rlpxConnection.expectMsgClass(classOf[Terminated])
  }

  it should "try to reconnect on broken rlpx connection" in new NodeStatusSetup with HandshakerSetup {
    override implicit lazy val system = ActorSystem("PeerActorSpec_System")
    override def protocol: Version = ProtocolVersions.PV63

    val time = new VirtualTime

    val peerMessageBus = system.actorOf(PeerEventBusActor.props)
    var rlpxConnection = TestProbe() // var as we actually need new instances
    val knownNodesManager = TestProbe()

    val peer = TestActorRef(
      Props(
        new PeerActor(
          new InetSocketAddress("127.0.0.1", 0),
          _ => {
            rlpxConnection = TestProbe()
            rlpxConnection.ref
          },
          peerConf,
          peerMessageBus,
          knownNodesManager.ref,
          false,
          Some(time.scheduler),
          handshaker
        )
      )
    )

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(hello: HelloEnc) =>
      ()
    }

    rlpxConnection.ref ! PoisonPill
    peer.unwatch(rlpxConnection.ref)
    time.advance(2.seconds)
    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
  }

  it should "successfully connect to ETC peer" in new TestSetup {
    val uri = new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray[Byte])}@localhost:9000")
    val completeUri = new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray[Byte])}@127.0.0.1:9000?discport=9000")
    peer ! PeerActor.ConnectTo(uri)
    peer ! PeerActor.ConnectTo(uri)

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    //Hello exchange
    val remoteHello = Hello(4, "test-client", Seq(Eth63Capability), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = ProtocolVersions.PV63,
      networkId = peerConf.networkId,
      totalDifficulty = daoForkBlockChainTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash
    )

    //Node status exchange
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    //Fork block exchange
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeadersEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))

    //Check that peer is connected
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Pong()))

    knownNodesManager.expectMsg(KnownNodesManager.AddKnownNode(completeUri))
    knownNodesManager.expectNoMessage()
  }

  it should "successfully connect to ETC peer with protocol 164" in new TestSetup {
    override def protocol: Version = ProtocolVersions.PV164
    val uri = new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray[Byte])}@localhost:9000")
    val completeUri = new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray[Byte])}@127.0.0.1:9000?discport=9000")
    peer ! PeerActor.ConnectTo(uri)
    peer ! PeerActor.ConnectTo(uri)

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    //Hello exchange
    val remoteHello = Hello(4, "test-client", Seq(Etc164Capability, Eth63Capability), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = PV164.Status(
      protocolVersion = ProtocolVersions.PV164,
      networkId = peerConf.networkId,
      chainWeight =
        ChainWeight.totalDifficultyOnly(daoForkBlockChainTotalDifficulty + 100000), // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash
    )

    //Node status exchange
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: PV164.Status.StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    //Fork block exchange
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeadersEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))

    //Check that peer is connected
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Pong()))

    knownNodesManager.expectMsg(KnownNodesManager.AddKnownNode(completeUri))
    knownNodesManager.expectNoMessage()
  }

  it should "successfully connect to and IPv6 peer" in new TestSetup {
    val uri = new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray[Byte])}@[::]:9000")
    val completeUri =
      new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray[Byte])}@[0:0:0:0:0:0:0:0]:9000?discport=9000")
    peer ! PeerActor.ConnectTo(uri)

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    //Hello exchange
    val remoteHello = Hello(4, "test-client", Seq(Eth63Capability), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = ProtocolVersions.PV63,
      networkId = peerConf.networkId,
      totalDifficulty = daoForkBlockChainTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash
    )

    //Node status exchange
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    //Fork block exchange
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeadersEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))

    //Check that peer is connected
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Pong()))

    knownNodesManager.expectMsg(KnownNodesManager.AddKnownNode(completeUri))
    knownNodesManager.expectNoMessage()
  }

  it should "disconnect from non-ETC peer" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    val remoteHello = Hello(4, "test-client", Seq(Eth63Capability), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val header =
      Fixtures.Blocks.ValidBlock.header
        .copy(difficulty = daoForkBlockChainTotalDifficulty + 100000, number = 3000000)
    storagesInstance.storages.appStateStorage
      .putBestBlockNumber(3000000) // after the fork
      .and(blockchain.storeBlockHeader(header))
      .and(storagesInstance.storages.blockNumberMappingStorage.put(3000000, header.hash))
      .commit()

    val remoteStatus = Status(
      protocolVersion = ProtocolVersions.PV63,
      networkId = peerConf.networkId,
      totalDifficulty = daoForkBlockChainTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash
    )

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeadersEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  it should "disconnect from non-ETC peer (when node is before fork)" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    val remoteHello = Hello(4, "test-client", Seq(Eth63Capability), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = ProtocolVersions.PV63,
      networkId = peerConf.networkId,
      totalDifficulty = daoForkBlockChainTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash
    )

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeadersEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))

    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  it should "disconnect on Hello timeout" in new TestSetup {
    val connection = TestProbe()

    peer ! PeerActor.HandleConnection(connection.ref, new InetSocketAddress("localhost", 9000))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.HandleConnection])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    time.advance(5.seconds)
    rlpxConnection.expectMsg(
      Timeouts.normalTimeout,
      RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.TimeoutOnReceivingAMessage))
    )

  }

  it should "respond to fork block request during the handshake" in new TestSetup {
    //Save dao fork block
    blockchain.storeBlockHeader(Fixtures.Blocks.DaoForkBlock.header).commit()

    //Handshake till EtcForkBlockExchangeState
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    val remoteHello = Hello(4, "test-client", Seq(Eth63Capability), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = ProtocolVersions.PV63,
      networkId = peerConf.networkId,
      totalDifficulty = daoForkBlockChainTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash
    )

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeadersEnc) => () }

    //Request dao fork block from the peer
    rlpxConnection.send(
      peer,
      RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Left(daoForkBlockNumber), 1, 0, false))
    )
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(Fixtures.Blocks.DaoForkBlock.header))))
  }

  it should "stash disconnect message until handshaked" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))
    peer ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers)

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    val remoteHello = Hello(4, "test-client", Seq(Eth63Capability), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = ProtocolVersions.PV63,
      networkId = peerConf.networkId,
      totalDifficulty = daoForkBlockChainTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash
    )

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeadersEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))

    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.TooManyPeers)))
  }

  it should "stay connected to pre fork peer" in new TestSetup {

    val remoteStatus = RemoteStatus(
      protocolVersion = ProtocolVersions.PV63,
      networkId = peerConf.networkId,
      chainWeight =
        ChainWeight.totalDifficultyOnly(daoForkBlockChainTotalDifficulty - 200000), // remote is before the fork
      bestHash = ByteString("blockhash"),
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )

    val peerActor = TestActorRef(
      Props(
        new PeerActor(
          new InetSocketAddress("127.0.0.1", 0),
          _ => rlpxConnection.ref,
          peerConf,
          peerMessageBus,
          knownNodesManager.ref,
          false,
          None,
          Mocks.MockHandshakerAlwaysSucceeds(remoteStatus, 0, false)
        )
      )
    )

    peerActor ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    rlpxConnection.send(peerActor, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: PongEnc) => () }
  }

  it should "disconnect gracefully after handshake" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    val remoteHello = Hello(4, "test-client", Seq(Eth63Capability), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = ProtocolVersions.PV63,
      networkId = peerConf.networkId,
      totalDifficulty = daoForkBlockChainTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash
    )

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeadersEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))

    //Test that the handshake succeeded
    val sender = TestProbe()(system)
    sender.send(peer, GetStatus)
    sender.expectMsg(StatusResponse(Handshaked))

    //Test peer terminated after peerConf.disconnectPoisonPillTimeout
    val manager = TestProbe()(system)
    manager.watch(peer)

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Disconnect(Reasons.Other)))

    // No terminated message instantly
    manager.expectNoMessage()

    // terminated only after peerConf.disconnectPoisonPillTimeout
    time.advance(peerConf.disconnectPoisonPillTimeout)

    manager.expectTerminated(peer)
  }

  trait BlockUtils {

    val blockBody = new BlockBody(Seq(), Seq())

    val etcForkBlockHeader = Fixtures.Blocks.DaoForkBlock.header

    val nonEtcForkBlockHeader =
      etcForkBlockHeader.copy(
        parentHash = ByteString("this"),
        ommersHash = ByteString("is"),
        beneficiary = ByteString("not"),
        stateRoot = ByteString("an"),
        transactionsRoot = ByteString("ETC"),
        receiptsRoot = ByteString("fork"),
        logsBloom = ByteString("block")
      )
  }

  trait NodeStatusSetup extends SecureRandomBuilder with EphemBlockchainTestSetup {
    override lazy val nodeKey = crypto.generateKeyPair(secureRandom)

    val nodeStatus =
      NodeStatus(key = nodeKey, serverStatus = ServerStatus.NotListening, discoveryStatus = ServerStatus.NotListening)

    val nodeStatusHolder = new AtomicReference(nodeStatus)

    val genesisBlock = Fixtures.Blocks.Genesis.block
    val genesisWeight = ChainWeight.totalDifficultyOnly(genesisBlock.header.difficulty)

    blockchain.save(genesisBlock, Nil, genesisWeight, saveAsBestBlock = true)

    val daoForkBlockNumber = 1920000

    val peerConf = new PeerConfiguration {
      override val fastSyncHostConfiguration: FastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = 200
        val maxBlocksBodiesPerMessage: Int = 200
        val maxReceiptsPerMessage: Int = 200
        val maxMptComponentsPerMessage: Int = 200
      }
      override val rlpxConfiguration: RLPxConfiguration = new RLPxConfiguration {
        override val waitForTcpAckTimeout: FiniteDuration = Timeouts.normalTimeout
        override val waitForHandshakeTimeout: FiniteDuration = Timeouts.normalTimeout
      }
      override val waitForHelloTimeout: FiniteDuration = 3 seconds
      override val waitForStatusTimeout: FiniteDuration = 30 seconds
      override val waitForChainCheckTimeout: FiniteDuration = 15 seconds
      override val connectMaxRetries: Int = 3
      override val connectRetryDelay: FiniteDuration = 1 second
      override val disconnectPoisonPillTimeout: FiniteDuration = 3 seconds
      override val minOutgoingPeers = 5
      override val maxOutgoingPeers = 10
      override val maxIncomingPeers = 5
      override val maxPendingPeers = 5
      override val pruneIncomingPeers = 0
      override val minPruneAge = 1.minute
      override val networkId: Int = 1

      override val updateNodesInitialDelay: FiniteDuration = 5.seconds
      override val updateNodesInterval: FiniteDuration = 20.seconds
      override val shortBlacklistDuration: FiniteDuration = 1.minute
      override val longBlacklistDuration: FiniteDuration = 3.minutes
      override val statSlotDuration: FiniteDuration = 1.minute
      override val statSlotCount: Int = 30
    }

  }

  trait HandshakerSetup extends NodeStatusSetup { self =>
    def protocol: Version

    val handshakerConfiguration = new EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = Some(
        new ForkResolver.EtcForkResolver(self.blockchainConfig.daoForkConfig.get)
      )
      override val nodeStatusHolder: AtomicReference[NodeStatus] = self.nodeStatusHolder
      override val peerConfiguration: PeerConfiguration = self.peerConf
      override val blockchain: Blockchain = self.blockchain
      override val appStateStorage: AppStateStorage = self.storagesInstance.storages.appStateStorage
      override val protocolVersion: Int = protocol
    }

    val handshaker = EtcHandshaker(handshakerConfiguration)
  }

  trait TestSetup extends NodeStatusSetup with BlockUtils with HandshakerSetup {
    override def protocol: Version = ProtocolVersions.PV63

    val genesisHash = genesisBlock.hash

    val daoForkBlockChainTotalDifficulty = BigInt("39490964433395682584")

    val rlpxConnection = TestProbe()

    val time = new VirtualTime

    val peerMessageBus = system.actorOf(PeerEventBusActor.props)

    val knownNodesManager = TestProbe()

    val peer = TestActorRef(
      Props(
        new PeerActor(
          new InetSocketAddress("127.0.0.1", 0),
          _ => rlpxConnection.ref,
          peerConf,
          peerMessageBus,
          knownNodesManager.ref,
          false,
          Some(time.scheduler),
          handshaker
        )
      )
    )
  }

}
