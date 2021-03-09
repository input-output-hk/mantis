package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.blockchain.sync.fast.DownloaderState
import io.iohk.ethereum.blockchain.sync.fast.SyncStateScheduler.SyncResponse
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor.{NoUsefulDataInResponse, ResponseProcessingResult, UnrequestedResponse, UsefulData}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV63.NodeData
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers

import java.net.InetSocketAddress

class SyncStateDownloaderStateSpec
    extends TestKit(ActorSystem("SyncStateDownloaderStateSpec_System"))
    with AnyFlatSpecLike
    with Matchers
    with BeforeAndAfterAll
    with WithActorSystemShutDown {

  "DownloaderState" should "schedule requests for retrieval" in new TestSetup {
    val newState = initialState.scheduleNewNodesForRetrieval(potentialNodesHashes)
    assert(newState.nodesToGet.size == potentialNodesHashes.size)
    assert(newState.nonDownloadedNodes.size == potentialNodesHashes.size)
    assert(potentialNodesHashes.forall(h => newState.nodesToGet.contains(h)))
  }

  it should "assign request to peers from already scheduled nodes to a max capacity" in new TestSetup {
    val perPeerCapacity = 20
    val newState = initialState.scheduleNewNodesForRetrieval(potentialNodesHashes)
    val (requests, newState1) = newState.assignTasksToPeers(peers, None, nodesPerPeerCapacity = perPeerCapacity)
    assert(requests.size == 3)
    assert(requests.forall(req => req.nodes.size == perPeerCapacity))
    assert(newState1.activeRequests.size == 3)
    assert(newState1.nonDownloadedNodes.size == potentialNodesHashes.size - (peers.size * perPeerCapacity))
    assert(
      requests.forall(request => request.nodes.forall(hash => newState1.nodesToGet(hash).contains(request.peer.id)))
    )
  }

  it should "favour already existing requests when assigning tasks with new requests" in new TestSetup {
    val perPeerCapacity = 20
    val (alreadyExistingTasks, newTasks) = potentialNodesHashes.splitAt(2 * perPeerCapacity)
    val newState = initialState.scheduleNewNodesForRetrieval(alreadyExistingTasks)
    val (requests, newState1) =
      newState.assignTasksToPeers(peers, Some(newTasks), nodesPerPeerCapacity = perPeerCapacity)
    assert(requests.size == 3)
    assert(requests.forall(req => req.nodes.size == perPeerCapacity))
    // all already existing task should endup in delivery
    assert(alreadyExistingTasks.forall(hash => newState1.nodesToGet(hash).isDefined))
    // check that first 20 nodes from new nodes has been schedued for delivery and next 40 is waiting for available peer
    assert(newTasks.take(perPeerCapacity).forall(hash => newState1.nodesToGet(hash).isDefined))
    assert(newTasks.drop(perPeerCapacity).forall(hash => newState1.nodesToGet(hash).isEmpty))

    // standard check that active requests are in line with nodes in delivery
    assert(newState1.activeRequests.size == 3)
    assert(newState1.nonDownloadedNodes.size == potentialNodesHashes.size - (peers.size * perPeerCapacity))
    assert(
      requests.forall(request => request.nodes.forall(hash => newState1.nodesToGet(hash).contains(request.peer.id)))
    )
  }

  it should "correctly handle incoming responses" in new TestSetup {
    val perPeerCapacity = 20
    val newState = initialState.scheduleNewNodesForRetrieval(potentialNodesHashes)
    val (requests, newState1) = newState.assignTasksToPeers(peers, None, nodesPerPeerCapacity = perPeerCapacity)
    assert(requests.size == 3)
    assert(requests.forall(req => req.nodes.size == perPeerCapacity))

    val (handlingResult, newState2) =
      newState1.handleRequestSuccess(requests(0).peer, NodeData(requests(0).nodes.map(h => hashNodeMap(h)).toList))

    val usefulData = expectUsefulData(handlingResult)
    assert(usefulData.responses.size == perPeerCapacity)
    assert(requests(0).nodes.forall(h => !newState2.nodesToGet.contains(h)))
    assert(newState2.activeRequests.size == 2)

    val (handlingResult1, newState3) =
      newState2.handleRequestSuccess(requests(1).peer, NodeData(requests(1).nodes.map(h => hashNodeMap(h)).toList))
    val usefulData1 = expectUsefulData(handlingResult1)
    assert(usefulData1.responses.size == perPeerCapacity)
    assert(requests(1).nodes.forall(h => !newState3.nodesToGet.contains(h)))
    assert(newState3.activeRequests.size == 1)

    val (handlingResult2, newState4) =
      newState3.handleRequestSuccess(requests(2).peer, NodeData(requests(2).nodes.map(h => hashNodeMap(h)).toList))

    val usefulData2 = expectUsefulData(handlingResult2)
    assert(usefulData2.responses.size == perPeerCapacity)
    assert(requests(2).nodes.forall(h => !newState4.nodesToGet.contains(h)))
    assert(newState4.activeRequests.isEmpty)
  }

  it should "ignore responses from not requested peers" in new TestSetup {
    val perPeerCapacity = 20
    val newState = initialState.scheduleNewNodesForRetrieval(potentialNodesHashes)
    val (requests, newState1) = newState.assignTasksToPeers(peers, None, nodesPerPeerCapacity = perPeerCapacity)
    assert(requests.size == 3)
    assert(requests.forall(req => req.nodes.size == perPeerCapacity))

    val (handlingResult, newState2) =
      newState1.handleRequestSuccess(notKnownPeer, NodeData(requests(0).nodes.map(h => hashNodeMap(h)).toList))
    assert(handlingResult == UnrequestedResponse)
    // check that all requests are unchanged
    assert(newState2.activeRequests.size == 3)
    assert(requests.forall { req =>
      req.nodes.forall(h => newState2.nodesToGet(h).contains(req.peer.id))
    })
  }

  it should "handle empty responses from from peers" in new TestSetup {
    val perPeerCapacity = 20
    val newState = initialState.scheduleNewNodesForRetrieval(potentialNodesHashes)
    val (requests, newState1) = newState.assignTasksToPeers(peers, None, nodesPerPeerCapacity = perPeerCapacity)
    assert(requests.size == 3)
    assert(requests.forall(req => req.nodes.size == perPeerCapacity))

    val (handlingResult, newState2) = newState1.handleRequestSuccess(requests(0).peer, NodeData(Seq()))
    assert(handlingResult == NoUsefulDataInResponse)
    assert(newState2.activeRequests.size == 2)
    // hashes are still in download queue but they are free to graby other peers
    assert(requests(0).nodes.forall(h => newState2.nodesToGet(h).isEmpty))
  }

  it should "handle response where part of data is malformed (bad hashes)" in new TestSetup {
    val perPeerCapacity = 20
    val goodResponseCap = perPeerCapacity / 2
    val newState = initialState.scheduleNewNodesForRetrieval(potentialNodesHashes)
    val (requests, newState1) = newState.assignTasksToPeers(
      NonEmptyList.fromListUnsafe(List(peer1)),
      None,
      nodesPerPeerCapacity = perPeerCapacity
    )
    assert(requests.size == 1)
    assert(requests.forall(req => req.nodes.size == perPeerCapacity))
    val peerRequest = requests.head
    val goodResponse = peerRequest.nodes.toList.take(perPeerCapacity / 2).map(h => hashNodeMap(h))
    val badResponse = (200 until 210).map(ByteString(_)).toList
    val (result, newState2) = newState1.handleRequestSuccess(requests(0).peer, NodeData(goodResponse ++ badResponse))

    val usefulData = expectUsefulData(result)
    assert(usefulData.responses.size == perPeerCapacity / 2)
    assert(newState2.activeRequests.isEmpty)
    // good responses where delivered and removed form request queue
    assert(peerRequest.nodes.toList.take(goodResponseCap).forall(h => !newState2.nodesToGet.contains(h)))
    // bad responses has been put back to map but without active peer
    assert(peerRequest.nodes.toList.drop(goodResponseCap).forall(h => newState2.nodesToGet.contains(h)))
    assert(peerRequest.nodes.toList.drop(goodResponseCap).forall(h => newState2.nodesToGet(h).isEmpty))
  }

  it should "handle response when there are spaces between delivered values" in new TestSetup {
    val values = List(ByteString(1), ByteString(2), ByteString(3), ByteString(4), ByteString(5))
    val hashes = values.map(kec256)
    val responses = hashes.zip(values).map(s => SyncResponse(s._1, s._2))

    val requested = NonEmptyList.fromListUnsafe(hashes)
    val received = NonEmptyList.fromListUnsafe(List(values(1), values(3)))
    val (toReschedule, delivered) = initialState.process(requested, received)

    assert(toReschedule == List(hashes(4), hashes(2), hashes(0)))
    assert(delivered == List(responses(1), responses(3)))
  }

  it should "handle response when there is larger gap between values" in new TestSetup {
    val values = List(ByteString(1), ByteString(2), ByteString(3), ByteString(4), ByteString(5))
    val hashes = values.map(kec256)
    val responses = hashes.zip(values).map(s => SyncResponse(s._1, s._2))

    val requested = NonEmptyList.fromListUnsafe(hashes)
    val received = NonEmptyList.fromListUnsafe(List(values(0), values(4)))
    val (toReschedule, delivered) = initialState.process(requested, received)

    assert(toReschedule == List(hashes(3), hashes(2), hashes(1)))
    assert(delivered == List(responses(0), responses(4)))
  }

  it should "handle response when only last value is delivered" in new TestSetup {
    val values = List(ByteString(1), ByteString(2), ByteString(3), ByteString(4), ByteString(5))
    val hashes = values.map(kec256)
    val responses = hashes.zip(values).map(s => SyncResponse(s._1, s._2))

    val requested = NonEmptyList.fromListUnsafe(hashes)
    val received = NonEmptyList.fromListUnsafe(List(values.last))
    val (toReschedule, delivered) = initialState.process(requested, received)

    assert(toReschedule == List(hashes(3), hashes(2), hashes(1), hashes(0)))
    assert(delivered == List(responses.last))
  }

  it should "handle response when only first value is delivered" in new TestSetup {
    val values = List(ByteString(1), ByteString(2), ByteString(3), ByteString(4), ByteString(5))
    val hashes = values.map(kec256)
    val responses = hashes.zip(values).map(s => SyncResponse(s._1, s._2))

    val requested = NonEmptyList.fromListUnsafe(hashes)
    val received = NonEmptyList.fromListUnsafe(List(values.head))
    val (toReschedule, delivered) = initialState.process(requested, received)
    assert(toReschedule == List(hashes(1), hashes(2), hashes(3), hashes(4)))
    assert(delivered == List(responses.head))
  }

  it should "handle response when only middle values are delivered" in new TestSetup {
    val values = List(ByteString(1), ByteString(2), ByteString(3), ByteString(4), ByteString(5))
    val hashes = values.map(kec256)
    val responses = hashes.zip(values).map(s => SyncResponse(s._1, s._2))

    val requested = NonEmptyList.fromListUnsafe(hashes)
    val received = NonEmptyList.fromListUnsafe(List(values(2), values(3)))
    val (toReschedule, delivered) = initialState.process(requested, received)
    assert(toReschedule == List(hashes(4), hashes(1), hashes(0)))
    assert(delivered == List(responses(2), responses(3)))
  }

  trait TestSetup {
    def expectUsefulData(result: ResponseProcessingResult): UsefulData =
      result match {
        case UnrequestedResponse    => fail()
        case NoUsefulDataInResponse => fail()
        case data @ UsefulData(_)   => data
      }

    val ref1 = TestProbe().ref
    val ref2 = TestProbe().ref
    val ref3 = TestProbe().ref
    val ref4 = TestProbe().ref

    val initialState = DownloaderState(Map.empty, Map.empty)
    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 1), ref1, incomingConnection = false)
    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 2), ref2, incomingConnection = false)
    val peer3 = Peer(new InetSocketAddress("127.0.0.1", 3), ref3, incomingConnection = false)
    val notKnownPeer = Peer(new InetSocketAddress("127.0.0.1", 4), ref4, incomingConnection = false)
    val peers = NonEmptyList.fromListUnsafe(List(peer1, peer2, peer3))
    val potentialNodes = (1 to 100).map(i => ByteString(i)).toList
    val potentialNodesHashes = potentialNodes.map(node => kec256(node))
    val hashNodeMap = potentialNodesHashes.zip(potentialNodes).toMap
  }

}
