package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress
import java.util.concurrent.ThreadLocalRandom

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.testkit.TestActor.AutoPilot
import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.util.ByteString

import scala.concurrent.duration._
import scala.util.Random

import org.scalactic.anyvals.PosInt
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.blockchain.sync.StateSyncUtils.MptNodeData
import io.iohk.ethereum.blockchain.sync.StateSyncUtils.TrieProvider
import io.iohk.ethereum.blockchain.sync.fast.SyncStateScheduler
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor.RestartRequested
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor.StartSyncingTo
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor.StateSyncFinished
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor.StateSyncStats
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor.WaitingForNewTargetBlock
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.network.EtcPeerManagerActor._
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.ETH63.GetNodeData.GetNodeDataEnc
import io.iohk.ethereum.network.p2p.messages.ETH63.NodeData
import io.iohk.ethereum.network.p2p.messages.ProtocolVersions
import io.iohk.ethereum.utils.Config

class StateSyncSpec
    extends TestKit(ActorSystem("StateSyncSpec"))
    with AnyFlatSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ScalaCheckPropertyChecks
    with WithActorSystemShutDown {

  // those tests are somewhat long running 3 successful evaluation should be fine
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(3))

  "StateSync" should "sync state to different tries" in new TestSetup() {
    forAll(ObjectGenerators.genMultipleNodeData(1000)) { nodeData =>
      val initiator = TestProbe()
      initiator.ignoreMsg { case SyncStateSchedulerActor.StateSyncStats(_, _) => true }
      val trieProvider = TrieProvider()
      val target = trieProvider.buildWorld(nodeData)
      setAutoPilotWithProvider(trieProvider)
      initiator.send(syncStateSchedulerActor, StartSyncingTo(target, 1))
      initiator.expectMsg(20.seconds, StateSyncFinished)
    }
  }

  it should "sync state to different tries when peers provide different set of data each time" in new TestSetup() {
    forAll(ObjectGenerators.genMultipleNodeData(1000)) { nodeData =>
      val initiator = TestProbe()
      initiator.ignoreMsg { case SyncStateSchedulerActor.StateSyncStats(_, _) => true }
      val trieProvider1 = TrieProvider()
      val target = trieProvider1.buildWorld(nodeData)
      setAutoPilotWithProvider(trieProvider1, partialResponseConfig)
      initiator.send(syncStateSchedulerActor, StartSyncingTo(target, 1))
      initiator.expectMsg(20.seconds, StateSyncFinished)
    }
  }

  it should "sync state to different tries when peer provide mixed responses" in new TestSetup() {
    forAll(ObjectGenerators.genMultipleNodeData(1000)) { nodeData =>
      val initiator = TestProbe()
      initiator.ignoreMsg { case SyncStateSchedulerActor.StateSyncStats(_, _) => true }
      val trieProvider1 = TrieProvider()
      val target = trieProvider1.buildWorld(nodeData)
      setAutoPilotWithProvider(trieProvider1, mixedResponseConfig)
      initiator.send(syncStateSchedulerActor, StartSyncingTo(target, 1))
      initiator.expectMsg(20.seconds, StateSyncFinished)
    }
  }

  it should "restart state sync when requested" in new TestSetup() {
    forAll(ObjectGenerators.genMultipleNodeData(1000)) { nodeData =>
      val initiator = TestProbe()
      val trieProvider1 = TrieProvider()
      val target = trieProvider1.buildWorld(nodeData)
      setAutoPilotWithProvider(trieProvider1)
      initiator.send(syncStateSchedulerActor, StartSyncingTo(target, 1))
      initiator.send(syncStateSchedulerActor, RestartRequested)
      initiator.fishForMessage(20.seconds) {
        case _: StateSyncStats        => false
        case WaitingForNewTargetBlock => true
      }
    }
  }

  it should "start state sync when receiving start signal while bloom filter is loading" in new TestSetup() {
    override def buildBlockChain(): (BlockchainReader, BlockchainImpl) = {
      val storages = getNewStorages.storages
      val blockchainMetadata = getNewBlockchainMetadata
      val blockchainReader = BlockchainReader(storages, blockchainMetadata)
      (blockchainReader, BlockchainImpl(storages, blockchainReader, blockchainMetadata))
    }

    val nodeData = (0 until 1000).map(i => MptNodeData(Address(i), None, Seq(), i))
    val initiator = TestProbe()
    initiator.ignoreMsg { case SyncStateSchedulerActor.StateSyncStats(_, _) => true }
    val trieProvider1 = TrieProvider()
    val target = trieProvider1.buildWorld(nodeData)
    setAutoPilotWithProvider(trieProvider1)
    initiator.send(syncStateSchedulerActor, StartSyncingTo(target, 1))
    initiator.expectMsg(20.seconds, StateSyncFinished)
  }

  class TestSetup extends EphemBlockchainTestSetup with TestSyncConfig {
    implicit override lazy val system: ActorSystem = StateSyncSpec.this.system
    type PeerConfig = Map[PeerId, PeerAction]
    val syncInit: TestProbe = TestProbe()

    val peerStatus: RemoteStatus = RemoteStatus(
      protocolVersion = ProtocolVersions.ETH63.version,
      networkId = 1,
      chainWeight = ChainWeight.totalDifficultyOnly(10000),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo: PeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      chainWeight = peerStatus.chainWeight,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number,
      bestBlockHash = peerStatus.bestHash
    )

    val trieProvider =
      new TrieProvider(blockchain, blockchainReader, getNewStorages.storages.evmCodeStorage, blockchainConfig)

    val peersMap: Map[Peer, PeerInfo] = (1 to 8).map { i =>
      (
        Peer(
          PeerId(s"peer$i"),
          new InetSocketAddress("127.0.0.1", i),
          TestProbe(i.toString).ref,
          incomingConnection = false
        ),
        initialPeerInfo
      )
    }.toMap

    val blacklist: Blacklist = CacheBasedBlacklist.empty(100)

    sealed trait PeerAction
    case object FullResponse extends PeerAction
    case object PartialResponse extends PeerAction
    case object NoResponse extends PeerAction

    val defaultPeerConfig: PeerConfig = peersMap.map { case (peer, _) =>
      peer.id -> FullResponse
    }

    val maxMptNodeRequest = 50
    val minMptNodeRequest = 20
    val partialResponseConfig: PeerConfig = peersMap.map { case (peer, _) =>
      peer.id -> PartialResponse
    }

    val mixedResponseConfig: PeerConfig = peersMap.map { case (peer, _) =>
      if (peer.remoteAddress.getPort <= 3) {
        peer.id -> FullResponse
      } else if (peer.remoteAddress.getPort > 3 && peer.remoteAddress.getPort <= 6) {
        peer.id -> PartialResponse
      } else {
        peer.id -> NoResponse
      }
    }

    val etcPeerManager: TestProbe = TestProbe()

    val peerEventBus: TestProbe = TestProbe()

    def setAutoPilotWithProvider(trieProvider: TrieProvider, peerConfig: PeerConfig = defaultPeerConfig): Unit =
      etcPeerManager.setAutoPilot(new AutoPilot {
        override def run(sender: ActorRef, msg: Any): AutoPilot =
          msg match {
            case SendMessage(msg: GetNodeDataEnc, peer) =>
              peerConfig(peer) match {
                case FullResponse =>
                  val responseMsg =
                    NodeData(trieProvider.getNodes(msg.underlyingMsg.mptElementsHashes.toList).map(_.data))
                  sender ! MessageFromPeer(responseMsg, peer)
                  this
                case PartialResponse =>
                  val random: ThreadLocalRandom = ThreadLocalRandom.current()
                  val elementsToServe = random.nextInt(minMptNodeRequest, maxMptNodeRequest + 1)
                  val toGet = msg.underlyingMsg.mptElementsHashes.toList.take(elementsToServe)
                  val responseMsg = NodeData(trieProvider.getNodes(toGet).map(_.data))
                  sender ! MessageFromPeer(responseMsg, peer)
                  this
                case NoResponse =>
                  this
              }

            case GetHandshakedPeers =>
              sender ! HandshakedPeers(peersMap)
              this
          }
      })

    override lazy val syncConfig: Config.SyncConfig = defaultSyncConfig.copy(
      peersScanInterval = 0.5.second,
      nodesPerRequest = maxMptNodeRequest,
      blacklistDuration = 1.second,
      peerResponseTimeout = 1.second,
      syncRetryInterval = 50.milliseconds
    )

    def buildBlockChain(): (BlockchainReader, BlockchainImpl) = {
      val storages = getNewStorages.storages
      val blockchainMetadata = getNewBlockchainMetadata
      (
        BlockchainReader(storages, blockchainMetadata),
        BlockchainImpl(
          storages,
          BlockchainReader(storages, blockchainMetadata),
          blockchainMetadata
        )
      )
    }

    def genRandomArray(): Array[Byte] = {
      val arr = new Array[Byte](32)
      Random.nextBytes(arr)
      arr
    }

    def genRandomByteString(): ByteString =
      ByteString.fromArrayUnsafe(genRandomArray())

    lazy val syncStateSchedulerActor: ActorRef = {
      val (blockchainReader, blockchain) = buildBlockChain()
      system.actorOf(
        SyncStateSchedulerActor.props(
          SyncStateScheduler(
            blockchain,
            blockchainReader,
            getNewStorages.storages.evmCodeStorage,
            getNewStorages.storages.nodeStorage,
            syncConfig.stateSyncBloomFilterSize
          ),
          syncConfig,
          etcPeerManager.ref,
          peerEventBus.ref,
          blacklist,
          system.scheduler
        )
      )
    }
  }

}
