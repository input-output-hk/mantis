package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress
import java.util.concurrent.ThreadLocalRandom

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestActor.AutoPilot
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.StateSyncUtils.{MptNodeData, TrieProvider}
import io.iohk.ethereum.blockchain.sync.fast.{SyncStateScheduler, SyncStateSchedulerActor}
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor.{RestartRequested, StartSyncingTo, StateSyncFinished, StateSyncStats, WaitingForNewTargetBlock}
import io.iohk.ethereum.db.dataSource.RocksDbDataSource.IterationError
import io.iohk.ethereum.domain.{Address, BlockchainImpl, ChainWeight}
import io.iohk.ethereum.network.EtcPeerManagerActor.{GetHandshakedPeers, HandshakedPeers, PeerInfo, SendMessage}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV63.GetNodeData.GetNodeDataEnc
import io.iohk.ethereum.network.p2p.messages.PV63.NodeData
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.{Peer, PeerId}
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.{Fixtures, ObjectGenerators, WithActorSystemShutDown}
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalactic.anyvals.PosInt
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._
import scala.util.Random

class StateSyncSpec
    extends TestKit(ActorSystem("StateSyncSpec"))
    with AnyFlatSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ScalaCheckPropertyChecks
    with WithActorSystemShutDown {

  // those tests are somewhat long running 3 successful evaluation should be fine
  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = PosInt(3))

  "StateSync" should "sync state to different tries" in new TestSetup() {
    forAll(ObjectGenerators.genMultipleNodeData(1000)) { nodeData =>
      val initiator = TestProbe()
      initiator.ignoreMsg { case SyncStateSchedulerActor.StateSyncStats(_, _) => true }
      val trieProvider = TrieProvider()
      val target = trieProvider.buildWorld(nodeData)
      setAutoPilotWithProvider(trieProvider)
      initiator.send(scheduler, StartSyncingTo(target, 1))
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
      initiator.send(scheduler, StartSyncingTo(target, 1))
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
      initiator.send(scheduler, StartSyncingTo(target, 1))
      initiator.expectMsg(20.seconds, StateSyncFinished)
    }
  }

  it should "restart state sync when requested" in new TestSetup() {
    forAll(ObjectGenerators.genMultipleNodeData(1000)) { nodeData =>
      val initiator = TestProbe()
      val trieProvider1 = TrieProvider()
      val target = trieProvider1.buildWorld(nodeData)
      setAutoPilotWithProvider(trieProvider1)
      initiator.send(scheduler, StartSyncingTo(target, 1))
      initiator.send(scheduler, RestartRequested)
      initiator.fishForMessage(20.seconds) {
        case _: StateSyncStats => false
        case WaitingForNewTargetBlock => true
      }
    }
  }

  it should "start state sync when receiving start signal while bloom filter is loading" in new TestSetup() {
    @volatile
    var loadingFinished = false
    val externalScheduler = Scheduler(system.dispatcher)

    override def buildBlockChain(): BlockchainImpl = {
      val storages = getNewStorages.storages

      new BlockchainImpl(
        blockHeadersStorage = storages.blockHeadersStorage,
        blockBodiesStorage = storages.blockBodiesStorage,
        blockNumberMappingStorage = storages.blockNumberMappingStorage,
        receiptStorage = storages.receiptStorage,
        evmCodeStorage = storages.evmCodeStorage,
        pruningMode = storages.pruningMode,
        nodeStorage = storages.nodeStorage,
        cachedNodeStorage = storages.cachedNodeStorage,
        chainWeightStorage = storages.chainWeightStorage,
        transactionMappingStorage = storages.transactionMappingStorage,
        appStateStorage = storages.appStateStorage,
        stateStorage = storages.stateStorage
      ) {
        override def mptStateSavedKeys(): Observable[Either[IterationError, ByteString]] = {
          Observable.interval(10.milliseconds).map(_ => Right(ByteString(1))).takeWhile(_ => !loadingFinished)
        }
      }

    }
    val nodeData = (0 until 1000).map(i => MptNodeData(Address(i), None, Seq(), i))
    val initiator = TestProbe()
    initiator.ignoreMsg { case SyncStateSchedulerActor.StateSyncStats(_, _) => true }
    val trieProvider1 = TrieProvider()
    val target = trieProvider1.buildWorld(nodeData)
    setAutoPilotWithProvider(trieProvider1)
    initiator.send(scheduler, StartSyncingTo(target, 1))
    externalScheduler.scheduleOnce(2.second) {
      loadingFinished = true
    }

    initiator.expectMsg(20.seconds, StateSyncFinished)
  }

  class TestSetup extends EphemBlockchainTestSetup with TestSyncConfig {
    override implicit lazy val system = StateSyncSpec.this.system
    type PeerConfig = Map[PeerId, PeerAction]
    val syncInit = TestProbe()

    val peerStatus = Status(
      protocolVersion = Versions.PV63,
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

    val trieProvider = new TrieProvider(blockchain, blockchainConfig)

    val peersMap = (1 to 8).map { i =>
      (
        Peer(new InetSocketAddress("127.0.0.1", i), TestProbe(i.toString).ref, incomingConnection = false),
        initialPeerInfo
      )
    }.toMap

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

    val etcPeerManager = TestProbe()

    val peerEventBus = TestProbe()

    def setAutoPilotWithProvider(trieProvider: TrieProvider, peerConfig: PeerConfig = defaultPeerConfig): Unit = {
      etcPeerManager.setAutoPilot(new AutoPilot {
        override def run(sender: ActorRef, msg: Any): AutoPilot = {
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
        }
      })
    }

    override lazy val syncConfig: Config.SyncConfig = defaultSyncConfig.copy(
      peersScanInterval = 0.5.second,
      nodesPerRequest = maxMptNodeRequest,
      blacklistDuration = 1.second,
      peerResponseTimeout = 1.second,
      syncRetryInterval = 50.milliseconds
    )

    def buildBlockChain() = {
      BlockchainImpl(getNewStorages.storages)
    }

    def genRandomArray(): Array[Byte] = {
      val arr = new Array[Byte](32)
      Random.nextBytes(arr)
      arr
    }

    def genRandomByteString(): ByteString = {
      ByteString.fromArrayUnsafe(genRandomArray())
    }

    lazy val scheduler = system.actorOf(
      SyncStateSchedulerActor.props(
        SyncStateScheduler(
          buildBlockChain(),
          syncConfig.stateSyncBloomFilterSize
        ),
        syncConfig,
        etcPeerManager.ref,
        peerEventBus.ref,
        system.scheduler
      )
    )
  }

}
