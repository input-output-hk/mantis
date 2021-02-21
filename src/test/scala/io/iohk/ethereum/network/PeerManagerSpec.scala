package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor._
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.domain.{Block, BlockBody, BlockHeader, ChainWeight}
import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, RemoteStatus}
import io.iohk.ethereum.network.PeerActor.{ConnectTo, PeerClosedConnection}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.PeerHandshaked
import io.iohk.ethereum.network.PeerEventBusActor.{PeerEvent, Publish, Subscribe}
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, PeerConfiguration, Peers, SendMessage}
import io.iohk.ethereum.network.discovery.{DiscoveryConfig, Node, PeerDiscoveryManager}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.ProtocolVersions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.{Fixtures, NormalPatience, WithActorSystemShutDown}
import org.bouncycastle.util.encoders.Hex
import org.scalatest.Inspectors
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}, Arbitrary.arbitrary
import scala.concurrent.duration._

// scalastyle:off magic.number
class PeerManagerSpec
    extends TestKit(ActorSystem("PeerManagerSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with Eventually
    with NormalPatience
    with ScalaCheckDrivenPropertyChecks {

  behavior of "PeerManagerActor"

  it should "try to connect to bootstrap and known nodes on startup" in new TestSetup {
    start()
    handleInitialNodesDiscovery()
  }

  it should "blacklist peer that fail to establish tcp connection" in new TestSetup {
    start()
    handleInitialNodesDiscovery()

    val probe: TestProbe = createdPeers(1).probe

    probe.expectMsgClass(classOf[PeerActor.ConnectTo])

    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection1.ref, incomingPeerAddress1)

    val probe2: TestProbe = createdPeers(2).probe
    val peer = Peer(incomingPeerAddress1, probe2.ref, incomingConnection = true)

    peerManager ! PeerClosedConnection(peer.remoteAddress.getHostString, Disconnect.Reasons.Other)

    eventually {
      peerManager.underlyingActor.blacklistedPeers.size shouldEqual 1
    }
  }

  it should "retry connections to remaining bootstrap nodes" in new TestSetup {
    start()
    handleInitialNodesDiscovery()

    val probe: TestProbe = createdPeers(1).probe

    probe.expectMsgClass(classOf[PeerActor.ConnectTo])

    probe.ref ! PoisonPill

    time.advance(21000) // wait for next scan

    eventually {
      peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    }
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))
  }

  it should "replace lost connections with random nodes" in new TestSetup {
    start()
    handleInitialNodesDiscovery()

    val probe: TestProbe = createdPeers.head.probe

    probe.expectMsgClass(classOf[PeerActor.ConnectTo])

    probe.ref ! PoisonPill

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetRandomNodeInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.RandomNodeInfo(bootstrapNodes.head))
  }

  it should "publish disconnect messages from peers" in new TestSetup {
    start()
    handleInitialNodesDiscovery()

    val probe: TestProbe = createdPeers(1).probe

    probe.ref ! PoisonPill

    time.advance(21000) // connect to 2 bootstrap peers

    peerEventBus.expectMsg(Publish(PeerDisconnected(PeerId(probe.ref.path.name))))
  }

  it should "not handle the connection from a peer that's already connected" in new TestSetup {
    start()
    handleInitialNodesDiscovery()

    val connection = TestProbe()

    val watcher = TestProbe()
    watcher.watch(connection.ref)

    peerManager ! PeerManagerActor.HandlePeerConnection(connection.ref, new InetSocketAddress("127.0.0.1", 30340))

    watcher.expectMsgClass(classOf[Terminated])
  }

  it should "handle pending and handshaked incoming peers" in new TestSetup {
    start()
    handleInitialNodesDiscovery()

    // There are 2 bootstrap nodes in the test config.
    createdPeers.head.probe.expectMsgClass(classOf[PeerActor.ConnectTo])
    createdPeers(1).probe.expectMsgClass(classOf[PeerActor.ConnectTo])

    time.advance(21000) // wait for next scan

    eventually {
      peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    }
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection1.ref, incomingPeerAddress1)

    // It should have created the next peer for the first incoming connection (probably using a synchronous test scheduler).
    val probe2: TestProbe = createdPeers(2).probe
    val peer = Peer(incomingPeerAddress1, probe2.ref, incomingConnection = true, Some(incomingNodeId1))
    probe2.expectMsg(PeerActor.HandleConnection(incomingConnection1.ref, incomingPeerAddress1))
    probe2.reply(PeerEvent.PeerHandshakeSuccessful(peer, initialPeerInfo))

    val watcher = TestProbe()
    watcher.watch(incomingConnection3.ref)

    // Try to connect with 2 more.
    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection2.ref, incomingPeerAddress2)
    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection3.ref, incomingPeerAddress3)

    // The second should be terminated because max-pending is 1.
    watcher.expectMsgClass(classOf[Terminated])

    // Simulate the successful handshake with the 2nd incoming. It should be disconnected because max-incoming is 1.
    val probe3: TestProbe = createdPeers(3).probe

    val secondPeer = Peer(incomingPeerAddress2, probe3.ref, incomingConnection = true, Some(incomingNodeId2))

    probe3.expectMsg(PeerActor.HandleConnection(incomingConnection2.ref, incomingPeerAddress2))
    probe3.reply(PeerEvent.PeerHandshakeSuccessful(secondPeer, initialPeerInfo))
    probe3.expectMsg(PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers))

    // Peer(3) after receiving disconnect schedules poison pill for himself
    probe3.ref ! PoisonPill

    peerEventBus.expectMsg(Publish(PeerDisconnected(PeerId(probe3.ref.path.name))))

    // TooManyPeers should also trigger a pruning cycle.
    peerStatistics.expectMsg(
      PeerStatisticsActor.GetStatsForAll(peerConfiguration.statSlotDuration * peerConfiguration.statSlotCount)
    )
    peerStatistics.reply(PeerStatisticsActor.StatsForAll(Map.empty))
    // There's only one connection that can be pruned.
    probe2.expectMsg(PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers))
  }

  it should "handle common message about getting peers" in new TestSetup {
    start()
    handleInitialNodesDiscovery()

    val requestSender = TestProbe()

    requestSender.send(peerManager, GetPeers)
    requestSender.expectMsgClass(classOf[Peers])
  }

  it should "handle common message about sending message to peer" in new TestSetup {
    start()
    handleInitialNodesDiscovery()

    val probe: TestProbe = createdPeers(1).probe

    probe.expectMsgClass(classOf[PeerActor.ConnectTo])

    val baseBlockHeader: BlockHeader = Fixtures.Blocks.Block3125369.header
    val header: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 4)
    val block = NewBlock(Block(header, BlockBody(Nil, Nil)), 300)

    peerManager ! SendMessage(block, PeerId(probe.ref.path.name))
    probe.expectMsg(PeerActor.SendMessage(block))
  }

  it should "disconnect from incoming peers already handshaked" in new TestSetup {
    start()
    handleInitialNodesDiscovery()

    // Finish handshake with the first of the bootstrap peers
    val TestPeer(peerAsOutgoing, peerAsOutgoingProbe) = createdPeers.head

    val ConnectTo(uriConnectedTo) = peerAsOutgoingProbe.expectMsgClass(classOf[PeerActor.ConnectTo])
    val nodeId = ByteString(Hex.decode(uriConnectedTo.getUserInfo))

    peerAsOutgoingProbe.reply(
      PeerEvent.PeerHandshakeSuccessful(peerAsOutgoing.copy(nodeId = Some(nodeId)), initialPeerInfo)
    )

    createdPeers(1).probe.expectMsgClass(classOf[PeerActor.ConnectTo])

    // Repeated incoming connection from one of the bootstrap peers
    val peerAsIncomingTcpConnection = incomingConnection1
    val peerAsIncomingAddress = incomingPeerAddress1

    peerManager ! PeerManagerActor.HandlePeerConnection(peerAsIncomingTcpConnection.ref, peerAsIncomingAddress)

    val peerAsIncomingProbe = createdPeers.last.probe
    val peerAsIncoming = Peer(peerAsIncomingAddress, peerAsIncomingProbe.ref, incomingConnection = true, Some(nodeId))

    peerAsIncomingProbe.expectMsg(
      PeerActor.HandleConnection(peerAsIncomingTcpConnection.ref, peerAsIncoming.remoteAddress)
    )
    peerAsIncomingProbe.reply(PeerEvent.PeerHandshakeSuccessful(peerAsIncoming, initialPeerInfo))

    peerAsIncomingProbe.expectMsg(PeerActor.DisconnectPeer(Disconnect.Reasons.AlreadyConnected))
  }

  it should "disconnect from outgoing peer if, while it was pending, the same peer hanshaked as incoming" in new TestSetup {
    start()
    handleInitialNodesDiscovery()

    // Keep both bootstrap peers as pending
    val TestPeer(peerAsOutgoing, peerAsOutgoingProbe) = createdPeers.head

    val ConnectTo(uriConnectedTo) = peerAsOutgoingProbe.expectMsgClass(classOf[PeerActor.ConnectTo])
    val nodeId = ByteString(Hex.decode(uriConnectedTo.getUserInfo))

    createdPeers(1).probe.expectMsgClass(classOf[PeerActor.ConnectTo])

    // Receive incoming connection from one of the bootstrap peers
    val peerAsIncomingTcpConnection = incomingConnection1
    val peerAsIncomingAddress = incomingPeerAddress1

    peerManager ! PeerManagerActor.HandlePeerConnection(peerAsIncomingTcpConnection.ref, peerAsIncomingAddress)

    val peerAsIncomingProbe = createdPeers.last.probe
    val peerAsIncoming = Peer(peerAsIncomingAddress, peerAsIncomingProbe.ref, incomingConnection = true, Some(nodeId))

    peerAsIncomingProbe.expectMsg(
      PeerActor.HandleConnection(peerAsIncomingTcpConnection.ref, peerAsIncoming.remoteAddress)
    )
    peerAsIncomingProbe.reply(PeerEvent.PeerHandshakeSuccessful(peerAsIncoming, initialPeerInfo))

    // Handshake with peer as outgoing is finished
    peerAsOutgoingProbe.reply(
      PeerEvent.PeerHandshakeSuccessful(peerAsOutgoing.copy(nodeId = Some(nodeId)), initialPeerInfo)
    )
    peerAsOutgoingProbe.expectMsg(PeerActor.DisconnectPeer(Disconnect.Reasons.AlreadyConnected))
  }

  behavior of "outgoingConnectionDemand"

  it should "try to connect to at least min-outgoing-peers but no more than max-outgoing-peers" in new ConnectedPeersFixture {
    forAll { (connectedPeers: ConnectedPeers) =>
      val demand = PeerManagerActor.outgoingConnectionDemand(connectedPeers, peerConfiguration)
      demand shouldBe >=(0)
      if (connectedPeers.outgoingHandshakedPeersCount >= peerConfiguration.minOutgoingPeers) {
        demand shouldBe 0
      } else {
        connectedPeers.outgoingPeersCount + demand shouldBe peerConfiguration.maxOutgoingPeers
      }
    }
  }

  it should "try to connect to discovered nodes if there's an outgoing demand: new nodes first, retried last" in new TestSetup {
    start()
    val discoveredNodes: Set[Node] = Set(
      "enode://111bd28d5b2c1378d748383fd83ff59572967c317c3063a9f475a26ad3f1517642a164338fb5268d4e32ea1cc48e663bd627dec572f1d201c7198518e5a506b1@88.99.216.30:45834?discport=45834",
      "enode://2b69a3926f36a7748c9021c34050be5e0b64346225e477fe7377070f6289bd363b2be73a06010fd516e6ea3ee90778dd0399bc007bb1281923a79374f842675a@51.15.116.226:30303?discport=30303"
    ).map(new java.net.URI(_)).map(Node.fromUri)

    peerManager ! PeerDiscoveryManager.DiscoveredNodesInfo(discoveredNodes)

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetRandomNodeInfo)

    val probe: TestProbe = createdPeers(0).probe
    probe.expectMsgClass(classOf[PeerActor.ConnectTo])

    val probe2: TestProbe = createdPeers(1).probe
    probe2.expectMsgClass(classOf[PeerActor.ConnectTo])

    peerManager ! PeerClosedConnection(discoveredNodes.head.addr.getHostAddress, Disconnect.Reasons.TooManyPeers)

    peerManager.underlyingActor.blacklistedPeers.size shouldEqual 1
    peerManager.underlyingActor.triedNodes.size shouldEqual 2

    time.advance(360000) // wait till the peer is out of the blacklist

    val newRoundDiscoveredNodes = discoveredNodes + Node.fromUri(
      new java.net.URI(
        "enode://a59e33ccd2b3e52d578f1fbd70c6f9babda2650f0760d6ff3b37742fdcdfdb3defba5d56d315b40c46b70198c7621e63ffa3f987389c7118634b0fefbbdfa7fd@51.158.191.43:38556?discport=38556"
      )
    )

    peerManager ! PeerDiscoveryManager.DiscoveredNodesInfo(newRoundDiscoveredNodes)

    probe.expectNoMessage()
    probe2.expectNoMessage()

    val probe3: TestProbe = createdPeers(2).probe
    probe3.expectMsgClass(classOf[PeerActor.ConnectTo])

    peerManager.underlyingActor.blacklistedPeers.size shouldEqual 0
    peerManager.underlyingActor.triedNodes.size shouldEqual 3
  }

  behavior of "numberOfIncomingConnectionsToPrune"

  it should "try to prune incoming connections down to the minimum allowed number" in new ConnectedPeersFixture {
    forAll { (connectedPeers: ConnectedPeers) =>
      val numPeersToPrune = PeerManagerActor.numberOfIncomingConnectionsToPrune(connectedPeers, peerConfiguration)
      numPeersToPrune shouldBe >=(0)
      numPeersToPrune shouldBe <=(peerConfiguration.pruneIncomingPeers)

      val minIncomingPeers = peerConfiguration.maxIncomingPeers - peerConfiguration.pruneIncomingPeers
      minIncomingPeers shouldBe >=(0)

      if (connectedPeers.incomingHandshakedPeersCount <= minIncomingPeers) {
        numPeersToPrune shouldBe 0
      } else {
        connectedPeers.incomingHandshakedPeersCount - numPeersToPrune shouldBe minIncomingPeers
      }
    }
  }

  behavior of "ConnectedPeers.prunePeers"

  // The `ConnectedPeers` is quite slow to generate, so doing a few tests in one go.
  it should "prune peers which are old enough, protecting against repeated forced pruning" in new ConnectedPeersFixture {
    forAll { (connectedPeers: ConnectedPeers) =>
      val numPeersToPrune = PeerManagerActor.numberOfIncomingConnectionsToPrune(connectedPeers, peerConfiguration)

      val now = System.currentTimeMillis

      // Prune the requested number of peers.
      {
        // Pretend we are in the future so age doesn't count.
        val (maxPrunedPeers, _) =
          connectedPeers.prunePeers(
            peerConfiguration.minPruneAge,
            numPeers = numPeersToPrune,
            currentTimeMillis = now + peerConfiguration.minPruneAge.toMillis + 1
          )

        maxPrunedPeers.size shouldBe numPeersToPrune
      }

      // Only prune peers which are old enough.
      {
        val (agedPrunedPeers, _) = connectedPeers.prunePeers(
          peerConfiguration.minPruneAge,
          numPeers = numPeersToPrune
        )
        Inspectors.forAll(agedPrunedPeers) {
          _.createTimeMillis shouldBe <=(now - peerConfiguration.minPruneAge.toMillis)
        }
      }

      // Not prune twice in a row within the prune cool-of time.
      {
        val now = System.currentTimeMillis
        val minAge = 1.minute
        // Check that we have at least 2 peers to prune.
        val (probe, _) = connectedPeers.prunePeers(
          minAge,
          numPeers = Int.MaxValue,
          currentTimeMillis = now
        )
        whenever(probe.size >= 2) {
          val (_, pruned1) = connectedPeers.prunePeers(minAge, numPeers = 1, currentTimeMillis = now)

          pruned1.prunePeers(minAge, numPeers = 1, currentTimeMillis = now + 1)._1 shouldBe empty

          pruned1
            .prunePeers(
              minAge,
              numPeers = 1,
              currentTimeMillis = now + minAge.toMillis
            )
            ._1 should not be empty
        }
      }

      // Not prune the same peer repeatedly.
      {
        val (peers1, pruned) = connectedPeers.prunePeers(
          peerConfiguration.minPruneAge,
          numPeers = numPeersToPrune
        )
        val (peers2, _) = pruned.prunePeers(
          peerConfiguration.minPruneAge,
          numPeers = numPeersToPrune,
          currentTimeMillis = now + peerConfiguration.minPruneAge.toMillis
        )
        peers1.toSet intersect peers2.toSet shouldBe empty
      }

      // Prune peers with minimum priority first.
      {
        val (peers, _) = connectedPeers.prunePeers(
          peerConfiguration.minPruneAge,
          numPeers = numPeersToPrune,
          priority = _.hashCode.toDouble // Dummy priority
        )
        whenever(peers.nonEmpty) {
          Inspectors.forAll(peers.init zip peers.tail) { case (a, b) =>
            a.id.hashCode shouldBe <=(b.id.hashCode)
          }
        }
      }
    }
  }

  it should "not prune again until the pruned peers are disconnected and new ones connect" in new ConnectedPeersFixture {
    val data = for {
      connectedPeers <- arbitrary[ConnectedPeers]
      numIncoming <- Gen.choose(0, peerConfiguration.pruneIncomingPeers)
      // Top up to max with new connections
      newIncoming <- Gen.listOfN(
        peerConfiguration.maxIncomingPeers - connectedPeers.incomingHandshakedPeersCount,
        genIncomingPeer
      )
    } yield (connectedPeers, newIncoming)

    forAll(data) { case (connectedPeers, newIncoming) =>
      val numPeersToPrune0 = PeerManagerActor.numberOfIncomingConnectionsToPrune(connectedPeers, peerConfiguration)

      // Not prune again until the peers have been disconnected.
      val (peers, pruning) = connectedPeers.prunePeers(
        peerConfiguration.minPruneAge,
        numPeersToPrune0
      )
      PeerManagerActor.numberOfIncomingConnectionsToPrune(pruning, peerConfiguration) shouldBe 0
      pruning.incomingPruningPeersCount shouldBe peers.size

      val pruned = peers.foldLeft(pruning) { case (ps, p) =>
        ps.removeTerminatedPeer(p.ref)._2
      }
      // Incoming connections should be at the minimum incoming peer count now.
      PeerManagerActor.numberOfIncomingConnectionsToPrune(pruned, peerConfiguration) shouldBe 0

      val replenished = newIncoming.foldLeft(pruned) { case (ps, p) =>
        ps.addNewPendingPeer(p).promotePeerToHandshaked(p)
      }
      // Incoming connections should be maxed out now, can prune again.
      PeerManagerActor.numberOfIncomingConnectionsToPrune(replenished, peerConfiguration) shouldBe >(0)
    }
  }

  behavior of "prunePriority"

  it should "calculate priority as count(responses)/lifetime" in {
    val now = System.currentTimeMillis

    def stat(responses: Int, firstSeen: FiniteDuration, lastSeen: FiniteDuration) =
      PeerStat.empty.copy(
        responsesReceived = responses,
        firstSeenTimeMillis = Some(now - firstSeen.toMillis),
        lastSeenTimeMillis = Some(now - lastSeen.toMillis)
      )

    val stats = Map(
      PeerId("Alice") -> stat(responses = 50, firstSeen = 1.hour, lastSeen = 50.minutes),
      PeerId("Bob") -> stat(responses = 100, firstSeen = 12.hours, lastSeen = 1.minute),
      PeerId("Charlie") -> stat(responses = 0, firstSeen = 20.hours, lastSeen = 5.minute).copy(requestsReceived = 1000)
    )

    val priority = PeerManagerActor.prunePriority(stats, now) _

    priority(PeerId("Alice")) shouldBe (50.0 / 1.hour.toMillis) +- 0.001
    priority(PeerId("Alice")) shouldBe >(priority(PeerId("Bob")))
    priority(PeerId("Charlie")) shouldBe 0.0
    priority(PeerId("Dave")) shouldBe 0.0
  }

  trait ConnectedPeersFixture {
    case class TestConfig(
        minOutgoingPeers: Int = 10,
        maxOutgoingPeers: Int = 30,
        maxIncomingPeers: Int = 30,
        maxPendingPeers: Int = 20,
        pruneIncomingPeers: Int = 20,
        minPruneAge: FiniteDuration = 30.minutes
    ) extends PeerManagerActor.PeerConfiguration.ConnectionLimits

    val peerConfiguration = TestConfig()

    implicit val arbConnectedPeers: Arbitrary[ConnectedPeers] = Arbitrary {
      genConnectedPeers(peerConfiguration.maxIncomingPeers, peerConfiguration.maxOutgoingPeers)
    }

  }

  trait TestSetup {
    val time = new VirtualTime

    case class TestPeer(peer: Peer, probe: TestProbe)
    var createdPeers: Seq[TestPeer] = Seq.empty

    val peerConfiguration: PeerConfiguration = Config.Network.peer
    val discoveryConfig = DiscoveryConfig(Config.config, Config.blockchains.blockchainConfig.bootstrapNodes)

    val peerDiscoveryManager = TestProbe()
    val peerEventBus = TestProbe()
    val knownNodesManager = TestProbe()
    val peerStatistics = TestProbe()

    val bootstrapNodes: Set[Node] =
      DiscoveryConfig(Config.config, Config.blockchains.blockchainConfig.bootstrapNodes).bootstrapNodes

    val knownNodes: Set[URI] = Set.empty

    val peerFactory: (ActorContext, InetSocketAddress, Boolean) => ActorRef = { (_, address, isIncoming) =>
      val peerProbe = TestProbe()
      createdPeers :+= TestPeer(Peer(address, peerProbe.ref, isIncoming), peerProbe)
      peerProbe.ref
    }

    val port = 30340
    val incomingConnection1 = TestProbe()
    val incomingNodeId1 = ByteString(1)
    val incomingPeerAddress1 = new InetSocketAddress("127.0.0.2", port)
    val incomingConnection2 = TestProbe()
    val incomingNodeId2 = ByteString(2)
    val incomingPeerAddress2 = new InetSocketAddress("127.0.0.3", port)
    val incomingConnection3 = TestProbe()
    val incomingNodeId3 = ByteString(3)
    val incomingPeerAddress3 = new InetSocketAddress("127.0.0.4", port)

    val peerStatus = RemoteStatus(
      protocolVersion = ProtocolVersions.PV63,
      networkId = 1,
      chainWeight = ChainWeight.totalDifficultyOnly(10000),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      chainWeight = peerStatus.chainWeight,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number,
      bestBlockHash = peerStatus.bestHash
    )

    val peerManager: TestActorRef[PeerManagerActor] = TestActorRef[PeerManagerActor](
      Props(
        new PeerManagerActor(
          peerEventBus.ref,
          peerDiscoveryManager.ref,
          peerConfiguration,
          knownNodesManager.ref,
          peerStatistics.ref,
          peerFactory,
          discoveryConfig,
          Some(time.scheduler)
        )
      )
    )(system)

    def start(): Unit = {
      peerEventBus.expectMsg(Subscribe(PeerHandshaked))

      peerManager ! PeerManagerActor.StartConnecting
    }

    def handleInitialNodesDiscovery(): Unit = {
      time.advance(6000) // wait for bootstrap nodes scan

      peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
      peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))
      knownNodesManager.expectMsg(KnownNodesManager.GetKnownNodes)
      knownNodesManager.reply(KnownNodesManager.KnownNodes(knownNodes))
    }
  }

  implicit val arbPeer: Arbitrary[Peer] = Arbitrary {
    for {
      ip <- Gen.listOfN(4, Gen.choose(0, 255)).map(_.mkString("."))
      port <- Gen.choose(10000, 60000)
      incoming <- arbitrary[Boolean]
      ageMillis <- Gen.choose(0, 24 * 60 * 60 * 1000)
    } yield Peer(
      remoteAddress = new InetSocketAddress(ip, port),
      ref = TestProbe().ref,
      incomingConnection = incoming,
      nodeId = None,
      createTimeMillis = System.currentTimeMillis - ageMillis
    )
  }

  val genIncomingPeer = arbitrary[Peer].map(_.copy(incomingConnection = true))
  val genOugoingPeer = arbitrary[Peer].map(_.copy(incomingConnection = false))

  def genConnectedPeers(
      maxIncomingPeers: Int,
      maxOutgoingPeers: Int
  ): Gen[ConnectedPeers] =
    for {
      numIncoming <- Gen.choose(0, maxIncomingPeers)
      numOutgoing <- Gen.choose(0, maxOutgoingPeers)
      incoming <- Gen.listOfN(numIncoming, genIncomingPeer)
      outgoing <- Gen.listOfN(numOutgoing, genOugoingPeer)
      connections0 = (incoming ++ outgoing).foldLeft(ConnectedPeers.empty)(_ addNewPendingPeer _)
      numHandshaked <- Gen.choose(0.75, 1.0).map(_ * (numIncoming + numOutgoing)).map(_.toInt)
      handshaked <- Gen.pick(numHandshaked, incoming ++ outgoing)
      connections1 = handshaked.foldLeft(connections0)(_ promotePeerToHandshaked _)
    } yield connections1

}
