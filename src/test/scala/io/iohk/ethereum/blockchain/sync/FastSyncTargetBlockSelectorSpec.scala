package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSyncTargetBlockSelector.Result
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class FastSyncTargetBlockSelectorSpec extends FlatSpec with Matchers with BeforeAndAfter with MockFactory {

  implicit var system: ActorSystem = _

  before {
    system = ActorSystem("SyncControllerSpec_System")
  }

  after {
    Await.result(system.terminate(), 1.seconds)
  }

  "FastSyncTargetBlockSelector" should "set target as 0 if best blocks received are less than target-block-offset" in new TestSetup {

    val handshakedPeersMsg = EtcPeerManagerActor.HandshakedPeers(Map(peer1 -> peer1Info, peer2 -> peer2Info))

    fastSyncTargetBlockSelector ! handshakedPeersMsg

    fastSyncTargetBlockSelector ! FastSyncTargetBlockSelector.ChooseTargetBlock

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false),
      peer1.id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = handshakedPeersMsg.peers(peer1).maxBlockNumber))), peer1.id))

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(ByteString("peer2_bestHash")), 1, 0, reverse = false),
      peer2.id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = handshakedPeersMsg.peers(peer2).maxBlockNumber))), peer2.id))

    parentActor.expectMsg(Result(None))
  }

  it should "set target as the max block received" in new TestSetup {

    val peer1MaxBlockNumber = syncConfig.targetBlockOffset + 1
    val peer2MaxBlockNumber = syncConfig.targetBlockOffset + 2

    val handshakedPeersMsg = EtcPeerManagerActor.HandshakedPeers(Map(
      peer1 -> peer1Info.copy(maxBlockNumber = peer1MaxBlockNumber),
      peer2 -> peer2Info.copy(maxBlockNumber = peer2MaxBlockNumber)
    ))

    fastSyncTargetBlockSelector ! handshakedPeersMsg

    fastSyncTargetBlockSelector ! FastSyncTargetBlockSelector.ChooseTargetBlock

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(ByteString("peer1_bestHash")), 1, 0, reverse = false),
      peer1.id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = peer1MaxBlockNumber))), peer1.id))

    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Right(ByteString("peer2_bestHash")), 1, 0, reverse = false),
      peer2.id))
    etcPeerManager.reply(MessageFromPeer(BlockHeaders(Seq(baseBlockHeader.copy(number = peer2MaxBlockNumber))), peer2.id))

    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(peer2MaxBlockNumber - syncConfig.targetBlockOffset), 1, 0, reverse = false), peer2.id)
    )
  }

  trait TestSetup extends SyncSpec {

    val peer1TestProbe: TestProbe = TestProbe()(system)
    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
    val peer1Status = Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer1Info = PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = syncConfig.targetBlockOffset - 1)

    val peer2TestProbe: TestProbe = TestProbe()(system)
    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 0), peer2TestProbe.ref, false)
    val peer2Status = Status(1, 1, 1, ByteString("peer2_bestHash"), ByteString("unused"))
    val peer2Info = PeerInfo(peer2Status, forkAccepted = true, totalDifficulty = peer2Status.totalDifficulty, maxBlockNumber = syncConfig.targetBlockOffset - 2)

    val etcPeerManager = TestProbe()
    etcPeerManager.ignoreMsg{
      case EtcPeerManagerActor.GetHandshakedPeers => true
    }

    val peerMessageBus = TestProbe()

    lazy val syncConfig = obtainSyncConfig()

    val parentActor = TestProbe()

    lazy val fastSyncTargetBlockSelector = TestActorRef(
      FastSyncTargetBlockSelector.props(
        etcPeerManager = etcPeerManager.ref,
        peerEventBus = peerMessageBus.ref,
        syncConfig = syncConfig,
        scheduler = system.scheduler
      ),
      parentActor.ref
    )
  }
}
