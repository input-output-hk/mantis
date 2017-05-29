package io.iohk.ethereum.network

import akka.actor.{ActorLogging, Actor}
import akka.util.ByteString
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.network.PeerActor.RLPxConnection
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler

trait BlockchainHost {
  self: Actor with ActorLogging =>

  val peerConfiguration: PeerConfiguration
  val blockchain: Blockchain

  def handleEvmMptFastDownload(rlpxConnection: RLPxConnection): Receive = {
    case RLPxConnectionHandler.MessageReceived(request: GetNodeData) =>
      val result: Seq[ByteString] = request.mptElementsHashes.take(peerConfiguration.fastSyncHostConfiguration.maxMptComponentsPerMessage).flatMap { hash =>
        blockchain.getMptNodeByHash(hash).map(_.toBytes: ByteString)
          .orElse(blockchain.getEvmCodeByHash(hash).map((evm: ByteString) => evm))
      }

      rlpxConnection.sendMessage(NodeData(result))
  }

  def handleBlockFastDownload(rlpxConnection: RLPxConnection): Receive = {
    case RLPxConnectionHandler.MessageReceived(request: GetReceipts) =>
      val receipts = request.blockHashes.take(peerConfiguration.fastSyncHostConfiguration.maxReceiptsPerMessage)
        .flatMap(hash => blockchain.getReceiptsByHash(hash))

      rlpxConnection.sendMessage(Receipts(receipts))

    case RLPxConnectionHandler.MessageReceived(request: GetBlockBodies) =>
      val blockBodies = request.hashes.take(peerConfiguration.fastSyncHostConfiguration.maxBlocksBodiesPerMessage)
        .flatMap(hash => blockchain.getBlockBodyByHash(hash))

      rlpxConnection.sendMessage(BlockBodies(blockBodies))

    case RLPxConnectionHandler.MessageReceived(request: GetBlockHeaders) =>
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

          rlpxConnection.sendMessage(BlockHeaders(blockHeaders))

        case _ => log.warning("got request for block headers with invalid block hash/number: {}", request)
      }
  }
}
