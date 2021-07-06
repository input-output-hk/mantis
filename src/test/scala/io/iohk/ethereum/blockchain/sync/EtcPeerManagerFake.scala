package io.iohk.ethereum.blockchain.sync
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.testkit.TestActor.AutoPilot
import akka.testkit.TestProbe
import akka.util.ByteString

import cats.effect.concurrent.Deferred

import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ReplaySubject
import monix.reactive.subjects.Subject

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.EtcPeerManagerActor
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.SendMessage
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockBodies
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockHeaders
import io.iohk.ethereum.network.p2p.messages.ETH62.GetBlockBodies
import io.iohk.ethereum.network.p2p.messages.ETH62.GetBlockHeaders
import io.iohk.ethereum.network.p2p.messages.ETH63.GetNodeData
import io.iohk.ethereum.network.p2p.messages.ETH63.GetReceipts
import io.iohk.ethereum.network.p2p.messages.ETH63.NodeData
import io.iohk.ethereum.network.p2p.messages.ETH63.Receipts
import io.iohk.ethereum.utils.Config.SyncConfig

class EtcPeerManagerFake(
    syncConfig: SyncConfig,
    peers: Map[Peer, PeerInfo],
    blocks: List[Block],
    getMptNodes: List[ByteString] => List[ByteString]
)(implicit system: ActorSystem, scheduler: Scheduler) {
  private val responsesSubject: Subject[MessageFromPeer, MessageFromPeer] = ReplaySubject()
  private val requestsSubject: Subject[SendMessage, SendMessage] = ReplaySubject()
  private val peersConnectedDeferred = Deferred.unsafe[Task, Unit]

  val probe: TestProbe = TestProbe("etc_peer_manager")
  val autoPilot =
    new EtcPeerManagerFake.EtcPeerManagerAutoPilot(
      requestsSubject,
      responsesSubject,
      peersConnectedDeferred,
      peers,
      blocks,
      getMptNodes
    )
  probe.setAutoPilot(autoPilot)

  def ref = probe.ref

  val requests: Observable[SendMessage] = requestsSubject
  val responses: Observable[MessageFromPeer] = responsesSubject
  val onPeersConnected: Task[Unit] = peersConnectedDeferred.get
  val pivotBlockSelected: Observable[BlockHeader] = responses
    .collect { case MessageFromPeer(BlockHeaders(Seq(header)), peer) =>
      (header, peer)
    }
    .bufferTumbling(peers.size)
    .concatMap { headersFromPeers =>
      val (headers, respondedPeers) = headersFromPeers.unzip

      if (headers.distinct.size == 1 && respondedPeers.toSet == peers.keySet.map(_.id)) {
        Observable.pure(headers.head)
      } else {
        Observable.empty
      }
    }

  val fetchedHeaders: Observable[Seq[BlockHeader]] = responses
    .collect {
      case MessageFromPeer(BlockHeaders(headers), _) if headers.size == syncConfig.blockHeadersPerRequest => headers
    }
  val fetchedBodies: Observable[Seq[BlockBody]] = responses
    .collect { case MessageFromPeer(BlockBodies(bodies), _) =>
      bodies
    }
  val requestedReceipts: Observable[Seq[ByteString]] = requests.collect(
    Function.unlift(msg =>
      msg.message.underlyingMsg match {
        case GetReceipts(hashes) => Some(hashes)
        case _                   => None
      }
    )
  )
  val fetchedBlocks: Observable[List[Block]] = fetchedBodies
    .scan[(List[Block], List[Block])]((Nil, blocks)) { case ((_, remainingBlocks), bodies) =>
      remainingBlocks.splitAt(bodies.size)
    }
    .map(_._1)
    .combineLatestMap(requestedReceipts)((blocks, _) => blocks) // a big simplification, but should be sufficient here

  val fetchedState: Observable[Seq[ByteString]] = responses.collect { case MessageFromPeer(NodeData(values), _) =>
    values
  }

}
object EtcPeerManagerFake {
  class EtcPeerManagerAutoPilot(
      requests: Subject[SendMessage, SendMessage],
      responses: Subject[MessageFromPeer, MessageFromPeer],
      peersConnected: Deferred[Task, Unit],
      peers: Map[Peer, PeerInfo],
      blocks: List[Block],
      getMptNodes: List[ByteString] => List[ByteString]
  )(implicit scheduler: Scheduler)
      extends AutoPilot {
    def run(sender: ActorRef, msg: Any): EtcPeerManagerAutoPilot = {
      msg match {
        case EtcPeerManagerActor.GetHandshakedPeers =>
          sender ! EtcPeerManagerActor.HandshakedPeers(peers)
          peersConnected.complete(()).onErrorHandle(_ => ()).runSyncUnsafe()
        case sendMsg @ EtcPeerManagerActor.SendMessage(rawMsg, peerId) =>
          requests.onNext(sendMsg)
          val response = rawMsg.underlyingMsg match {
            case GetBlockHeaders(startingBlock, maxHeaders, skip, false) =>
              val headers = blocks.tails
                .find(_.headOption.exists(blockMatchesStart(_, startingBlock)))
                .toList
                .flatten
                .zipWithIndex
                .collect { case (block, index) if index % (skip + 1) == 0 => block }
                .take(maxHeaders.toInt)
                .map(_.header)
              BlockHeaders(headers)

            case GetBlockBodies(hashes) =>
              val bodies = hashes.flatMap(hash => blocks.find(_.hash == hash)).map(_.body)
              BlockBodies(bodies)

            case GetReceipts(blockHashes) =>
              Receipts(blockHashes.map(_ => Nil))

            case GetNodeData(mptElementsHashes) =>
              NodeData(getMptNodes(mptElementsHashes.toList))
          }
          val theResponse = MessageFromPeer(response, peerId)
          sender ! theResponse
          responses.onNext(theResponse)
      }
      this
    }

    def blockMatchesStart(block: Block, startingBlock: Either[BigInt, ByteString]): Boolean =
      startingBlock.fold(nr => block.number == nr, hash => block.hash == hash)
  }
}
