package io.iohk.ethereum.network

import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerActor.SendMessage
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63._

trait BlockchainHost { _: EtcMessageHandler =>

  def handleEvmMptFastDownload: PartialFunction[Message, Unit] = {
    case request: GetNodeData =>

      val result: Seq[ByteString] = request.mptElementsHashes.take(peerConfiguration.fastSyncHostConfiguration.maxMptComponentsPerMessage).flatMap { hash =>
        blockchain.getMptNodeByHash(hash).map(_.toBytes: ByteString)
          .orElse(blockchain.getEvmCodeByHash(hash).map((evm: ByteString) => evm))
      }

      peer.ref ! SendMessage(NodeData(result))
  }

  def handleBlockFastDownload: PartialFunction[Message, Unit] = {
    case request: GetReceipts =>
      val receipts = request.blockHashes.take(peerConfiguration.fastSyncHostConfiguration.maxReceiptsPerMessage)
        .flatMap(hash => blockchain.getReceiptsByHash(hash))

      peer.ref ! SendMessage(Receipts(receipts))

    case request: GetBlockBodies =>
      val blockBodies = request.hashes.take(peerConfiguration.fastSyncHostConfiguration.maxBlocksBodiesPerMessage)
        .flatMap(hash => blockchain.getBlockBodyByHash(hash))

      peer.ref ! SendMessage(BlockBodies(blockBodies))

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

          peer.ref ! SendMessage(BlockHeaders(blockHeaders))

        case _ => log.warn("got request for block headers with invalid block hash/number: {}", request)
      }

  }
}
