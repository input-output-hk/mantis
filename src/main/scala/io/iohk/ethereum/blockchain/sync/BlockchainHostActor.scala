package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageReceivedClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, GetReceipts, NodeData, Receipts}
import io.iohk.ethereum.network.{PeerId, PeerManagerActor}

class BlockchainHostActor(blockchain: Blockchain, peerConfiguration: PeerConfiguration,
                          peerEventBusActor: ActorRef, peerManagerActor: ActorRef) extends Actor with ActorLogging {

  private val requestMsgsCodes = Set(GetNodeData.code, GetReceipts.code, GetBlockBodies.code, GetBlockHeaders.code)
  peerEventBusActor ! Subscribe(MessageReceivedClassifier(requestMsgsCodes, PeerSelector.AllPeers))

  override def receive: Receive = {
    case MessageFromPeer(message, peerId) =>
      val handleBlockchainRequest = handleBlockFastDownload(peerId) orElse handleEvmMptFastDownload(peerId)
      handleBlockchainRequest(message)
  }

  private def handleEvmMptFastDownload(peerId: PeerId): PartialFunction[Message, Unit] = {
    case request: GetNodeData =>

      val result: Seq[ByteString] = request.mptElementsHashes.take(peerConfiguration.fastSyncHostConfiguration.maxMptComponentsPerMessage).flatMap { hash =>
        blockchain.getMptNodeByHash(hash).map(_.toBytes: ByteString)
          .orElse(blockchain.getEvmCodeByHash(hash).map((evm: ByteString) => evm))
      }

      peerManagerActor ! PeerManagerActor.SendMessage(NodeData(result), peerId)
  }

  private def handleBlockFastDownload(peerId: PeerId): PartialFunction[Message, Unit] = {
    case request: GetReceipts =>
      val receipts = request.blockHashes.take(peerConfiguration.fastSyncHostConfiguration.maxReceiptsPerMessage)
        .flatMap(hash => blockchain.getReceiptsByHash(hash))

      peerManagerActor ! PeerManagerActor.SendMessage(Receipts(receipts), peerId)

    case request: GetBlockBodies =>
      val blockBodies = request.hashes.take(peerConfiguration.fastSyncHostConfiguration.maxBlocksBodiesPerMessage)
        .flatMap(hash => blockchain.getBlockBodyByHash(hash))

      peerManagerActor ! PeerManagerActor.SendMessage(BlockBodies(blockBodies), peerId)

    case request: GetBlockHeaders =>
      val blockNumber = request.block.fold(a => Some(a), b => blockchain.getBlockHeaderByHash(b).map(_.number))

      blockNumber match {
        case Some(startBlockNumber) if startBlockNumber >= 0 && request.maxHeaders >= 0 && request.skip >= 0 =>

          val headersCount: BigInt =
            if (peerConfiguration.fastSyncHostConfiguration.maxBlocksHeadersPerMessage < request.maxHeaders)
              peerConfiguration.fastSyncHostConfiguration.maxBlocksHeadersPerMessage
            else
              request.maxHeaders

          val range = if (request.reverse) {
            startBlockNumber to (startBlockNumber - (request.skip + 1) * headersCount + 1) by -(request.skip + 1)
          } else {
            startBlockNumber to (startBlockNumber + (request.skip + 1) * headersCount - 1) by (request.skip + 1)
          }

          val blockHeaders: Seq[BlockHeader] = range.flatMap { a: BigInt => blockchain.getBlockHeaderByNumber(a) }

          peerManagerActor ! PeerManagerActor.SendMessage(BlockHeaders(blockHeaders), peerId)

        case _ => log.warning("got request for block headers with invalid block hash/number: {}", request)
      }

  }

}

object BlockchainHostActor {

  def props(blockchain: Blockchain, peerConfiguration: PeerConfiguration,
            peerEventBusActor: ActorRef, peerManagerActor: ActorRef): Props =
    Props(new BlockchainHostActor(blockchain, peerConfiguration, peerEventBusActor, peerManagerActor))

}
