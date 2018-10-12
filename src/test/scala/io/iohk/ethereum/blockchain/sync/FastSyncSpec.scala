package io.iohk.ethereum.blockchain.sync

import akka.actor.{ ActorRef, ActorSystem, Cancellable, Terminated }
import akka.testkit.{ TestActorRef, TestProbe }
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.UnblacklistPeer
import io.iohk.ethereum.blockchain.sync.FastSync.SyncState
import io.iohk.ethereum.db.storage.{ AppStateStorage, FastSyncStateStorage }
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.EtcPeerManagerActor.{ HandshakedPeers, PeerInfo, SendMessage }
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{ MessageFromPeer, PeerDisconnected }
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{ MessageClassifier, PeerDisconnectedClassifier }
import io.iohk.ethereum.network.PeerEventBusActor.{ PeerSelector, Subscribe, Unsubscribe }
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62.{ BlockHeaders, GetBlockHeaders }
import io.iohk.ethereum.network.{ EtcPeerManagerActor, Peer, PeerId }
import io.iohk.ethereum.utils.Config.SyncConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.{ Matchers, WordSpec }

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

class FastSyncSpec extends WordSpec with Matchers with Eventually {

  "FastSync" when {

    "receive peer list messages" should {
      "add peer to handshakedPeers if got HandshakedPeers message" in new FastSyncTestSetup {
        etcPeerManager.send(fastSync, HandshakedPeers(handshakedPeers))
        peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer1Id))))
        fastSync.underlyingActor.handshakedPeers shouldBe handshakedPeers

        stopFastSync()
      }

      "remove peer from handshakedPeers if got PeerDisconnected message" in new FastSyncTestSetup {
        val classifier = PeerDisconnectedClassifier(PeerSelector.WithId(peer1Id))

        etcPeerManager.send(fastSync, HandshakedPeers(handshakedPeers))
        peerEventBus.expectMsg(Subscribe(classifier))
        fastSync.underlyingActor.handshakedPeers shouldBe handshakedPeers

        etcPeerManager.send(fastSync, PeerDisconnected(peer1Id))
        peerEventBus.expectMsg(Unsubscribe(classifier))

        fastSync.underlyingActor.handshakedPeers shouldBe Map.empty

        stopFastSync()
      }
    }

    "receive blacklist messages" should {
      "remove peer from blacklistedPeers if got UnblacklistPeer message" in new FastSyncTestSetup {
        import scala.concurrent.ExecutionContext.Implicits._
        val cancellable: Cancellable =
          system.scheduler.scheduleOnce(syncConfig.blacklistDuration, fastSync, UnblacklistPeer(peer1Id))(global)
        fastSync.underlyingActor.blacklistedPeers.put(peer1Id, cancellable)
        fastSync.underlyingActor.blacklistedPeers shouldBe mutable.LinkedHashMap((peer1Id, cancellable))

        fastSync ! UnblacklistPeer(peer1Id)
        fastSync.underlyingActor.blacklistedPeers shouldBe mutable.LinkedHashMap.empty

        stopFastSync()
      }
    }

    "receive start message" should {
      "start synchronization from scratch" in new FastSyncTestSetup {
        val firstNewBlock: Int = defaultExpectedTargetBlock + 1
        val newBlocks: Seq[BlockHeader] = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)

        // Peers for FastSync
        etcPeerManager.send(fastSync, HandshakedPeers(handshakedPeers))
        peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer1Id))))
        fastSync.underlyingActor.handshakedPeers shouldBe handshakedPeers

        requestSender.send(fastSync, FastSync.Start)

        // Check data storage
        val maybeSyncState: Option[SyncState] = storage.getSyncState()
        maybeSyncState.isDefined shouldBe false

        // Check target block selector
        val maybeTargetBlockSelector: Option[ActorRef] = getSingleChild(fastSync.underlyingActor.TargetBlockSelectorName)
        maybeTargetBlockSelector.isDefined shouldBe true

        val actualSelector: ActorRef = maybeTargetBlockSelector.get

        // Peers for target block selector
        etcPeerManager.send(actualSelector, HandshakedPeers(handshakedPeers))
        peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer1Id))))

        // Wait for scheduled second ChooseTargetBlock message
        time.advance(syncConfig.startRetryInterval)

        // Messages from target block selector
        etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(peer1Status.bestHash), 1, 0, reverse = false), peer1Id))
        val messageClassifier = MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1Id))
        peerEventBus.expectMsg(Subscribe(messageClassifier))
        peerEventBus.reply(MessageFromPeer(BlockHeaders(newBlocks), peer1Id))
        peerEventBus.expectMsg(Unsubscribe(messageClassifier))

        // Check storage
        val maybeStateStorage: Option[ActorRef] = getSingleChild(fastSync.underlyingActor.StateStorageName)
        maybeStateStorage.isDefined shouldBe true

        stopFastSync()
      }

      "start synchronization from specific state" in new FastSyncTestSetup {
        val newSafeTarget: Int = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
        val bestBlockNumber: Int = defaultExpectedTargetBlock
        val firstNewBlock: Int = bestBlockNumber + 1
        val newBlocks: Seq[BlockHeader] = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)

        val syncState: SyncState =
          defaultState.copy(bestBlockHeaderNumber = bestBlockNumber, safeDownloadTarget = newSafeTarget)

        // Peers for FastSync
        etcPeerManager.send(fastSync, HandshakedPeers(handshakedPeers))
        peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer1Id))))
        fastSync.underlyingActor.handshakedPeers shouldBe handshakedPeers

        storage.putSyncState(syncState)
        storage.getSyncState().foreach{ state =>
          state shouldBe syncState
          state.updatingTargetBlock shouldBe false
        }

        requestSender.send(fastSync, FastSync.Start)

        // Normally handling new messages
        etcPeerManager.expectMsg(SendMessage(GetBlockHeaders(Left(firstNewBlock), newBlocks.size, 0, reverse = false), peer1Id))
        peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer1Id))))
        peerEventBus.expectMsg(Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer1Id))))
        peerEventBus.reply(MessageFromPeer(BlockHeaders(newBlocks), peer1Id))
        peerEventBus.expectMsg(Unsubscribe())

        stopFastSync()
      }
    }

    "fastSync was interrupted during update" should {
      "choose new target block" in new FastSyncTestSetup {
        val newSafeTarget: Int = defaultExpectedTargetBlock + syncConfig.fastSyncBlockValidationX
        val bestBlockNumber: Int = defaultExpectedTargetBlock
        val firstNewBlock: Int = bestBlockNumber + 1
        val newBlocks: Seq[BlockHeader] = getHeaders(firstNewBlock, syncConfig.blockHeadersPerRequest)

        val syncState: SyncState = defaultState.copy(updatingTargetBlock = true)

        // Peers for FastSync
        etcPeerManager.send(fastSync, HandshakedPeers(handshakedPeers))
        peerEventBus.expectMsg(Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer1Id))))
        fastSync.underlyingActor.handshakedPeers shouldBe handshakedPeers

        storage.putSyncState(syncState)
        storage.getSyncState().foreach{ state =>
          state shouldBe syncState
          state.updatingTargetBlock shouldBe true
        }

        requestSender.send(fastSync, FastSync.Start)

        // Check if target-block-selector was created
        val maybeTargetBlockSelector: Option[ActorRef] = getSingleChild(fastSync.underlyingActor.TargetBlockSelectorName)
        maybeTargetBlockSelector.isDefined shouldBe true

        stopFastSync()
      }
    }
  }

  // scalastyle:off magic.number
  trait FastSyncTestSetup extends MockFactory with SyncFixtures with EphemBlockchainTestSetup {

    override lazy val syncConfig = SyncConfig(
      doFastSync = true,

      printStatusInterval = 1.hour,
      persistStateSnapshotInterval = 20.seconds,
      targetBlockOffset = 500,
      branchResolutionRequestSize = 20,
      blacklistDuration = 5.seconds,
      syncRetryInterval = 1.second,
      checkForNewBlockInterval = 1.second,
      startRetryInterval = 500.milliseconds,
      blockChainOnlyPeersPoolSize = 100,
      maxConcurrentRequests = 10,
      blockHeadersPerRequest = 10,
      blockBodiesPerRequest = 10,
      nodesPerRequest = 10,
      receiptsPerRequest = 10,
      minPeersToChooseTargetBlock = 1,
      peerResponseTimeout = 1.second,
      peersScanInterval = 500.milliseconds,
      fastSyncThrottle = 100.milliseconds,
      maxQueuedBlockNumberAhead = 10,
      maxQueuedBlockNumberBehind = 10,
      maxNewBlockHashAge = 20,
      maxNewHashes = 64,
      broadcastNewBlockHashes = true,
      redownloadMissingStateNodes = false,
      fastSyncBlockValidationK = 100,
      fastSyncBlockValidationN = 2048,
      fastSyncBlockValidationX = 10,
      maxTargetDifference =  5,
      maximumTargetUpdateFailures = 1
    )

    override implicit lazy val system: ActorSystem = ActorSystem("FastSyncSpec_System")

    val peerEventBus = TestProbe()
    val etcPeerManager = TestProbe()
    etcPeerManager.ignoreMsg{
      case EtcPeerManagerActor.SendMessage(msg, _) if isNewBlock(msg.underlyingMsg) => true
      case EtcPeerManagerActor.GetHandshakedPeers => true
    }

    val time = new VirtualTime

    val storage: FastSyncStateStorage = storagesInstance.storages.fastSyncStateStorage
    val appStorage: AppStateStorage = storagesInstance.storages.appStateStorage
    val fastSync: TestActorRef[FastSync] = TestActorRef[FastSync](FastSync.props(
      storage,
      appStorage,
      blockchain,
      validators,
      peerEventBus.ref,
      etcPeerManager.ref,
      syncConfig,
      time.scheduler
    ))

    val peer1TestProbe: TestProbe = TestProbe("peer1")
    val peer1: Peer = mkPeer(1, peer1TestProbe)
    val peer1Id: PeerId = peer1.id
    val peer1Status: Status = mkPeerStatus(1)
    val peer1Info: PeerInfo = mkPeerInfo(peer1Status)
    val handshakedPeers = Map(peer1 -> peer1Info)

    val requestSender = TestProbe()

    def stopFastSync(): Terminated = Await.result(system.terminate(), 1.second)

    def getSingleChild(name: String): Option[ActorRef] = Try(fastSync.getSingleChild(name)).toOption
  }
}
