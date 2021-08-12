package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableFor3
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.blockchain.sync.PeerListSupportNg.PeerWithInfo
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.Capability
import org.scalatest.prop.TableFor3

class PeersClientSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  import Peers._
  val table: TableFor3[Map[PeerId,PeerWithInfo],Option[Peer],String] = Table[Map[PeerId, PeerWithInfo], Option[Peer], String](
    ("PeerInfo map", "Expected best peer", "Scenario info (selected peer)"),
    (
      Map(),
      None,
      "No peers"
    ),
    (
      Map(peer1.id -> PeerWithInfo(peer1, peerInfo(0, 100, fork = false))),
      None,
      "Single peer"
    ),
    (
      Map(
        peer1.id -> PeerWithInfo(peer1, peerInfo(0, 100, fork = false)),
        peer2.id -> PeerWithInfo(peer2, peerInfo(0, 50, fork = true))
      ),
      Some(peer2),
      "Peer2 with lower TD but following the ETC fork"
    ),
    (
      Map(peer1.id -> PeerWithInfo(peer1, peerInfo(0, 100)), peer2.id -> PeerWithInfo(peer2, peerInfo(0, 101))),
      Some(peer2),
      "Peer2 with higher TD"
    ),
    (
      Map(
        peer1.id -> PeerWithInfo(peer1, peerInfo(0, 100)),
        peer2.id -> PeerWithInfo(peer2, peerInfo(0, 101)),
        peer3.id -> PeerWithInfo(peer3, peerInfo(1, 50))
      ),
      Some(peer3),
      "Peer3 with lower TD but higher checkpoint number"
    ),
    (
      Map(
        peer1.id -> PeerWithInfo(peer1, peerInfo(0, 100)),
        peer2.id -> PeerWithInfo(peer2, peerInfo(4, 101)),
        peer3.id -> PeerWithInfo(peer3, peerInfo(4, 50))
      ),
      Some(peer2),
      "Peer2 with equal checkpoint number and higher TD"
    )
  )

  "PeerClient" should "determine the best peer based on its latest checkpoint number and total difficulty" in {
    forAll(table) { (peerInfoMap, expectedPeer, _) =>
      PeersClient.bestPeer(peerInfoMap) shouldEqual expectedPeer
    }
  }

  it should "determine the next best peer (same as  bestPeer when lru set is empty)" in {
    forAll(table) { (peerInfoMap, expectedPeer, _) =>
      PeersClient.nextBestPeer(peerInfoMap) shouldEqual expectedPeer
    }
  }

  it should "determine the next best peer when lru is used" in {
    val table = Table[Map[PeerId, PeerWithInfo], Option[Peer], Option[Peer], String](
      ("PeerInfo map", "Used best peer", "Expected best peer", "Scenario info (selected peer)"),
      (
        Map(),
        None,
        None,
        "No peers"
      ),
      (
        Map(peer1.id -> PeerWithInfo(peer1, peerInfo(0, 100, fork = false))),
        None,
        None,
        "Single peer"
      ),
      (
        Map(
          peer1.id -> PeerWithInfo(peer1, peerInfo(0, 100, fork = false)),
          peer2.id -> PeerWithInfo(peer2, peerInfo(0, 50, fork = true))
        ),
        Some(peer2),
        None,
        "Peer2 with lower TD but following the ETC fork"
      ),
      (
        Map(peer1.id -> PeerWithInfo(peer1, peerInfo(0, 100)), peer2.id -> PeerWithInfo(peer2, peerInfo(0, 101))),
        Some(peer2),
        Some(peer1),
        "Peer2 with higher TD"
      ),
      (
        Map(
          peer1.id -> PeerWithInfo(peer1, peerInfo(0, 100)),
          peer2.id -> PeerWithInfo(peer2, peerInfo(0, 101)),
          peer3.id -> PeerWithInfo(peer3, peerInfo(1, 50))
        ),
        Some(peer3),
        Some(peer2),
        "Peer3 with lower TD but higher checkpoint number"
      ),
      (
        Map(
          peer1.id -> PeerWithInfo(peer1, peerInfo(0, 100)),
          peer2.id -> PeerWithInfo(peer2, peerInfo(4, 101)),
          peer3.id -> PeerWithInfo(peer3, peerInfo(4, 50))
        ),
        Some(peer2),
        Some(peer3),
        "Peer2 with equal checkpoint number and higher TD"
      )
    )
    forAll(table) { (peerInfoMap, usedPeer, expectedPeer, _) =>
      usedPeer.map(PeersClient.activeFetchingNodes.add)
      PeersClient.nextBestPeer(peerInfoMap) shouldEqual expectedPeer
    }
  }

  object Peers {
    implicit val system: ActorSystem = ActorSystem("PeersClient_System")

    val peer1: Peer = Peer(PeerId("peer1"), new InetSocketAddress("127.0.0.1", 1), TestProbe().ref, false)
    val peer2: Peer = Peer(PeerId("peer2"), new InetSocketAddress("127.0.0.1", 2), TestProbe().ref, false)
    val peer3: Peer = Peer(PeerId("peer3"), new InetSocketAddress("127.0.0.1", 3), TestProbe().ref, false)

    private val peerStatus = RemoteStatus(
      capability = Capability.ETH63,
      networkId = 1,
      chainWeight = ChainWeight(0, 0),
      bestHash = ByteString.empty,
      genesisHash = ByteString.empty
    )

    def peerInfo(chkp: Int, td: Int, fork: Boolean = true): PeerInfo =
      PeerInfo(
        peerStatus,
        ChainWeight(chkp, td),
        forkAccepted = fork,
        maxBlockNumber = 42,
        bestBlockHash = ByteString.empty
      )
  }
}
