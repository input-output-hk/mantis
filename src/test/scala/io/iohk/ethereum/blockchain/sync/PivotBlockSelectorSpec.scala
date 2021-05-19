package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.blockchain.sync.fast.PivotBlockSelector
import io.iohk.ethereum.blockchain.sync.fast.PivotBlockSelector.{Result, SelectPivotBlock}
import io.iohk.ethereum.domain.{BlockHeader, ChainWeight}
import io.iohk.ethereum.network.EtcPeerManagerActor.{HandshakedPeers, PeerInfo, RemoteStatus}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.{Codes, ProtocolVersions}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.{Fixtures, WithActorSystemShutDown}
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class PivotBlockSelectorSpec
    extends TestKit(ActorSystem("FastSyncPivotBlockSelectorSpec_System"))
    with AnyFlatSpecLike
    with Matchers
    with BeforeAndAfter
    with WithActorSystemShutDown {

  "FastSyncPivotBlockSelector" should "download pivot block from peers" in new TestSetup {
    updateHandshakedPeers(HandshakedPeers(threeAcceptedPeers))

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer2.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer3.id)
    )

    // Collecting pivot block (for voting)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer1.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer2.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer3.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )

    fastSync.expectMsg(Result(pivotBlockHeader))
    peerMessageBus.expectMsg(Unsubscribe())
  }

  it should "ask for the block number 0 if [bestPeerBestBlockNumber < syncConfig.pivotBlockOffset]" in new TestSetup {
    val highestNumber = syncConfig.pivotBlockOffset - 1

    updateHandshakedPeers(
      HandshakedPeers(
        threeAcceptedPeers
          .updated(peer1, threeAcceptedPeers(peer1).copy(maxBlockNumber = highestNumber))
          .updated(peer2, threeAcceptedPeers(peer2).copy(maxBlockNumber = highestNumber / 2))
          .updated(peer3, threeAcceptedPeers(peer3).copy(maxBlockNumber = highestNumber / 5))
      )
    )

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(0), 1, 0, reverse = false), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(0), 1, 0, reverse = false), peer2.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(0), 1, 0, reverse = false), peer3.id)
    )
  }

  it should "retry if there are no enough peers" in new TestSetup {
    updateHandshakedPeers(HandshakedPeers(singlePeer))

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectNoMessage()

    updateHandshakedPeers(HandshakedPeers(threeAcceptedPeers))

    time.advance(syncConfig.startRetryInterval)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )
  }

  it should "retry if there are no enough votes for one block" in new TestSetup {
    updateHandshakedPeers(HandshakedPeers(threeAcceptedPeers))

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer2.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer3.id)
    )

    // Collecting pivot block (for voting)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer1.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer2.id)

    // one peer return different header
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(differentBlockHeader)), peer3.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id))),
      Unsubscribe()
    )

    fastSync.expectNoMessage() // consensus not reached - process have to be repeated

    time.advance(syncConfig.startRetryInterval)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )
  }

  it should "find out that there are no enough votes as soon as possible" in new TestSetup {
    updateHandshakedPeers(HandshakedPeers(threeAcceptedPeers))

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer2.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer3.id)
    )

    // Collecting pivot block (for voting)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer1.id)

    // One peer return different header. Because pivotBlockSelector waits only for one peer more - consensus won't be reached
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(differentBlockHeader)), peer2.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Unsubscribe()
    )

    fastSync.expectNoMessage() // consensus not reached - process have to be repeated

    time.advance(syncConfig.startRetryInterval)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )
  }

  it should "handle case when one peer responded with wrong block header" in new TestSetup {
    override def minPeersToChoosePivotBlock: Int = 1

    updateHandshakedPeers(HandshakedPeers(singlePeer))

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id)))
    )

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer1.id)
    )

    // peer responds with block header number
    pivotBlockSelector ! MessageFromPeer(
      BlockHeaders(Seq(pivotBlockHeader.copy(number = expectedPivotBlock + 1))),
      peer1.id
    )

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Unsubscribe()
    )
    time.advance(syncConfig.syncRetryInterval)

    fastSync.expectNoMessage() // consensus not reached - process have to be repeated
    peerMessageBus.expectNoMessage()
  }

  it should "not ask additional peers if not needed" in new TestSetup {
    override val minPeersToChoosePivotBlock = 2
    override val peersToChoosePivotBlockMargin = 1

    updateHandshakedPeers(HandshakedPeers(allPeers))

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )
    peerMessageBus.expectNoMessage()

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer2.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer3.id)
    )
    etcPeerManager.expectNoMessage()

    // Collecting pivot block (for voting)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer1.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer2.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer3.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id))),
      Unsubscribe()
    )
    peerMessageBus.expectNoMessage()

    fastSync.expectMsg(Result(pivotBlockHeader))
  }

  it should "ask additional peers if needed" in new TestSetup {
    override val minPeersToChoosePivotBlock = 2
    override val peersToChoosePivotBlockMargin = 1

    updateHandshakedPeers(HandshakedPeers(allPeers))

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )
    peerMessageBus.expectNoMessage()

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer2.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer3.id)
    )
    etcPeerManager.expectNoMessage()

    // Collecting pivot block (for voting)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer1.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(differentBlockHeader)), peer2.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(anotherDifferentBlockHeader)), peer3.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id))),
      Subscribe(
        MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer4.id))
      ) // Next peer will be asked
    )

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer4.id)
    )
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer4.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer4.id))),
      Unsubscribe()
    )
    peerMessageBus.expectNoMessage()

    fastSync.expectMsg(Result(pivotBlockHeader))
  }

  it should "restart whole process after checking additional nodes" in new TestSetup {
    override val minPeersToChoosePivotBlock = 2
    override val peersToChoosePivotBlockMargin = 1

    updateHandshakedPeers(HandshakedPeers(allPeers))

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )
    peerMessageBus.expectNoMessage()

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer2.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer3.id)
    )
    etcPeerManager.expectNoMessage()

    // Collecting pivot block (for voting)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer1.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(differentBlockHeader)), peer2.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(anotherDifferentBlockHeader)), peer3.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id))),
      Subscribe(
        MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer4.id))
      ) // Next peer will be asked
    )

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer4.id)
    )
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(nextAnotherDifferentBlockHeader)), peer4.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer4.id))),
      Unsubscribe()
    )

    fastSync.expectNoMessage() // consensus not reached - process have to be repeated

    time.advance(syncConfig.startRetryInterval)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )
    peerMessageBus.expectNoMessage()
  }

  it should "check only peers with the highest block at least equal to [bestPeerBestBlockNumber - syncConfig.pivotBlockOffset]" in new TestSetup {
    updateHandshakedPeers(
      HandshakedPeers(allPeers.updated(peer1, allPeers(peer1).copy(maxBlockNumber = expectedPivotBlock - 1)))
    )

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer4.id)))
    )
    peerMessageBus.expectNoMessage() // Peer 1 will be skipped
  }

  it should "only use only peers from the correct network to choose pivot block" in new TestSetup() {
    updateHandshakedPeers(HandshakedPeers(peersFromDifferentNetworks))

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      // Peer 2 is skipped
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer4.id)))
    )
    peerMessageBus.expectNoMessage()

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer3.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(expectedPivotBlock), 1, 0, reverse = false), peer4.id)
    )

    // Collecting pivot block (for voting)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer1.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer3.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(pivotBlockHeader)), peer4.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer4.id)))
    )

    fastSync.expectMsg(Result(pivotBlockHeader))
    peerMessageBus.expectMsg(Unsubscribe())
  }

  it should "retry pivot block election with fallback to lower peer numbers" in new TestSetup {

    override val minPeersToChoosePivotBlock = 2
    override val peersToChoosePivotBlockMargin = 1

    updateHandshakedPeers(
      HandshakedPeers(
        allPeers
          .updated(peer1, allPeers(peer1).copy(maxBlockNumber = 2000))
          .updated(peer2, allPeers(peer2).copy(maxBlockNumber = 800))
          .updated(peer3, allPeers(peer3).copy(maxBlockNumber = 900))
          .updated(peer4, allPeers(peer4).copy(maxBlockNumber = 1400))
      )
    )

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer4.id)))
    )

    etcPeerManager.expectMsgAllOf(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(1400), 1, 0, reverse = false), peer1.id),
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(1400), 1, 0, reverse = false), peer4.id)
    )
    etcPeerManager.expectNoMessage()

    // Collecting pivot block (for voting)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 1400))), peer1.id)
    pivotBlockSelector ! MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = 1400))), peer4.id)

    peerMessageBus.expectMsgAllOf(
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer4.id))),
      Unsubscribe()
    )
    peerMessageBus.expectNoMessage()

    fastSync.expectMsg(Result(baseBlockHeader.copy(number = 1400)))
  }

  it should "restart pivot block selection after `maxPivotBlockFailuresCount` is reached" in new TestSetup {

    override val minPeersToChoosePivotBlock = 2
    override val peersToChoosePivotBlockMargin = 1

    updateHandshakedPeers(
      HandshakedPeers(
        allPeers
          .updated(peer1, allPeers(peer1).copy(maxBlockNumber = 2000))
          .updated(peer2, allPeers(peer2).copy(maxBlockNumber = 800))
          .updated(peer3, allPeers(peer3).copy(maxBlockNumber = 900))
          .updated(peer4, allPeers(peer4).copy(maxBlockNumber = 1000))
      )
    )

    pivotBlockSelector ! SelectPivotBlock

    peerMessageBus.expectNoMessage()

    updateHandshakedPeers(HandshakedPeers(threeAcceptedPeers))

    time.advance(syncConfig.startRetryInterval)

    peerMessageBus.expectMsgAllOf(
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer1.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer2.id))),
      Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer3.id)))
    )
  }

  class TestSetup extends TestSyncConfig {

    val blacklist: Blacklist = CacheBasedBlacklist.empty(100)

    private def isNewBlock(msg: Message): Boolean = msg match {
      case _: NewBlock => true
      case _ => false
    }

    val etcPeerManager = TestProbe()
    etcPeerManager.ignoreMsg {
      case EtcPeerManagerActor.SendMessage(msg, _) if isNewBlock(msg.underlyingMsg) => true
      case EtcPeerManagerActor.GetHandshakedPeers => true
    }

    val peerMessageBus = TestProbe()
    peerMessageBus.ignoreMsg {
      case Subscribe(MessageClassifier(codes, PeerSelector.AllPeers))
          if codes == Set(Codes.NewBlockCode, Codes.NewBlockHashesCode) =>
        true
      case Subscribe(PeerDisconnectedClassifier(_)) => true
      case Unsubscribe(Some(PeerDisconnectedClassifier(_))) => true
    }

    def minPeersToChoosePivotBlock = 3
    def peersToChoosePivotBlockMargin = 1

    override def defaultSyncConfig: SyncConfig = super.defaultSyncConfig.copy(
      doFastSync = true,
      branchResolutionRequestSize = 30,
      checkForNewBlockInterval = 1.second,
      blockHeadersPerRequest = 10,
      blockBodiesPerRequest = 10,
      minPeersToChoosePivotBlock = minPeersToChoosePivotBlock,
      peersToChoosePivotBlockMargin = peersToChoosePivotBlockMargin,
      peersScanInterval = 500.milliseconds,
      peerResponseTimeout = 2.seconds,
      redownloadMissingStateNodes = false,
      fastSyncBlockValidationX = 10,
      blacklistDuration = 1.second
    )

    val fastSync = TestProbe()
    val time = new VirtualTime

    lazy val pivotBlockSelector: ActorRef = system.actorOf(
      PivotBlockSelector.props(
        etcPeerManager.ref,
        peerMessageBus.ref,
        defaultSyncConfig,
        time.scheduler,
        fastSync.ref,
        blacklist
      )
    )

    val baseBlockHeader = Fixtures.Blocks.Genesis.header

    val bestBlock = 400000
    // Ask for pivot block header (the best block from the best peer - offset)
    val expectedPivotBlock = bestBlock - syncConfig.pivotBlockOffset

    val pivotBlockHeader: BlockHeader = baseBlockHeader.copy(number = expectedPivotBlock)
    val differentBlockHeader: BlockHeader =
      baseBlockHeader.copy(number = expectedPivotBlock, extraData = ByteString("different"))
    val anotherDifferentBlockHeader: BlockHeader =
      baseBlockHeader.copy(number = expectedPivotBlock, extraData = ByteString("different2"))
    val nextAnotherDifferentBlockHeader: BlockHeader =
      baseBlockHeader.copy(number = expectedPivotBlock, extraData = ByteString("different3"))

    val peer1TestProbe: TestProbe = TestProbe("peer1")(system)
    val peer2TestProbe: TestProbe = TestProbe("peer2")(system)
    val peer3TestProbe: TestProbe = TestProbe("peer3")(system)
    val peer4TestProbe: TestProbe = TestProbe("peer4")(system)

    val peer1 = Peer(PeerId("peer1"), new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
    val peer2 = Peer(PeerId("peer2"), new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref, false)
    val peer3 = Peer(PeerId("peer3"), new InetSocketAddress("127.0.0.3", 0), peer3TestProbe.ref, false)
    val peer4 = Peer(PeerId("peer4"), new InetSocketAddress("127.0.0.4", 0), peer4TestProbe.ref, false)

    val peer1Status =
      RemoteStatus(
        ProtocolVersions.PV164,
        1,
        ChainWeight.totalDifficultyOnly(20),
        ByteString("peer1_bestHash"),
        ByteString("unused")
      )
    val peer2Status = peer1Status.copy(bestHash = ByteString("peer2_bestHash"))
    val peer3Status = peer1Status.copy(bestHash = ByteString("peer3_bestHash"))
    val peer4Status = peer1Status.copy(bestHash = ByteString("peer4_bestHash"))

    val allPeers = Map(
      peer1 -> PeerInfo(
        peer1Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer1Status.bestHash
      ),
      peer2 -> PeerInfo(
        peer2Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer2Status.bestHash
      ),
      peer3 -> PeerInfo(
        peer3Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer3Status.bestHash
      ),
      peer4 -> PeerInfo(
        peer4Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer4Status.bestHash
      )
    )

    val threeAcceptedPeers = Map(
      peer1 -> PeerInfo(
        peer1Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer1Status.bestHash
      ),
      peer2 -> PeerInfo(
        peer2Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer2Status.bestHash
      ),
      peer3 -> PeerInfo(
        peer3Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer3Status.bestHash
      )
    )

    val singlePeer = Map(
      peer1 -> PeerInfo(
        peer1Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer1Status.bestHash
      )
    )

    val peersFromDifferentNetworks = Map(
      peer1 -> PeerInfo(
        peer1Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer1Status.bestHash
      ),
      peer2 -> PeerInfo(
        peer2Status,
        forkAccepted = false,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer2Status.bestHash
      ),
      peer3 -> PeerInfo(
        peer3Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer3Status.bestHash
      ),
      peer4 -> PeerInfo(
        peer4Status,
        forkAccepted = true,
        chainWeight = peer1Status.chainWeight,
        maxBlockNumber = bestBlock,
        bestBlockHash = peer4Status.bestHash
      )
    )

    def updateHandshakedPeers(handshakedPeers: HandshakedPeers): Unit = pivotBlockSelector ! handshakedPeers
  }
}
