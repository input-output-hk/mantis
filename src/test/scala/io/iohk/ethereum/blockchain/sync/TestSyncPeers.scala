package io.iohk.ethereum.blockchain.sync
import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, RemoteStatus}
import io.iohk.ethereum.network.{Peer, PeerId}
import io.iohk.ethereum.network.p2p.messages.ProtocolVersions

trait TestSyncPeers { self: TestSyncConfig =>
  implicit def system: ActorSystem

  val peer1TestProbe: TestProbe = TestProbe("peer1")(system)
  val peer2TestProbe: TestProbe = TestProbe("peer2")(system)
  val peer3TestProbe: TestProbe = TestProbe("peer3")(system)

  val peer1 = Peer(PeerId("peer1"), new InetSocketAddress("127.0.0.1", 0), peer1TestProbe.ref, false)
  val peer2 = Peer(PeerId("peer2"), new InetSocketAddress("127.0.0.2", 0), peer2TestProbe.ref, false)
  val peer3 = Peer(PeerId("peer3"), new InetSocketAddress("127.0.0.3", 0), peer3TestProbe.ref, false)

  val peer1Status =
    RemoteStatus(
      ProtocolVersions.PV164,
      1,
      ChainWeight.totalDifficultyOnly(20),
      ByteString("peer1_bestHash"),
      ByteString("unused")
    )
  val peer2Status = peer1Status.copy(bestHash = ByteString("peer2_bestHash"))

  val bestBlock = 400000
  val expectedPivotBlock = bestBlock - syncConfig.pivotBlockOffset

  val defaultPeer1Info = PeerInfo(
    peer1Status,
    forkAccepted = true,
    chainWeight = peer1Status.chainWeight,
    maxBlockNumber = bestBlock,
    bestBlockHash = peer1Status.bestHash
  )

  val twoAcceptedPeers = Map(
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
}
