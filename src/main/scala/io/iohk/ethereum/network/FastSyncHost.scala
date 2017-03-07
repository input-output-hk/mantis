package io.iohk.ethereum.network

import akka.actor.Actor
import akka.event.LoggingAdapter
import akka.util.ByteString
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.network.PeerActor.{PeerConfiguration, RLPxConnection}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler

trait FastSyncHost {
  val peerConfiguration: PeerConfiguration
  val storage: Blockchain

  def handleEvmMptFastDownload(rlpxConnection: RLPxConnection): Actor.Receive = {
    case RLPxConnectionHandler.MessageReceived(request: GetNodeData) =>
      import io.iohk.ethereum.rlp.encode

      val result: Seq[ByteString] = request.mptElementsHashes.take(peerConfiguration.fastSyncHostConfiguration.maxMptComponentsPerMessage).flatMap { hash =>
        storage.getMptNodeByHash(hash).map((node: MptNode) => ByteString(encode[MptNode](node)))
          .orElse(storage.getEvmCodeByHash(hash).map((evm: ByteString) => evm))
      }

      rlpxConnection.sendMessage(NodeData(result))
  }

  def handleBlockFastDownload(rlpxConnection: RLPxConnection, log: LoggingAdapter): Actor.Receive = {
    case RLPxConnectionHandler.MessageReceived(request: GetReceipts) =>
      val receipts = request.blockHashes.take(peerConfiguration.fastSyncHostConfiguration.maxReceiptsPerMessage)
        .flatMap(hash => storage.getReceiptsByHash(hash))

      rlpxConnection.sendMessage(Receipts(receipts))

    case RLPxConnectionHandler.MessageReceived(request: GetBlockBodies) =>
      val blockBodies = request.hashes.take(peerConfiguration.fastSyncHostConfiguration.maxBlocksBodiesPerMessage)
        .flatMap(hash => storage.getBlockBodyByHash(hash))

      rlpxConnection.sendMessage(BlockBodies(blockBodies))

    case RLPxConnectionHandler.MessageReceived(request: GetBlockHeaders) =>
      val blockNumber = request.block.fold(a => Some(a), b => storage.getBlockHeaderByHash(b).map(_.number))

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

          val blockHeaders: Seq[BlockHeader] = range.flatMap { a: BigInt => storage.getBlockHeaderByNumber(a) }

          rlpxConnection.sendMessage(BlockHeaders(blockHeaders))

        case _ => log.info("got request for block headers with invalid block hash/number: {}", request)
      }
  }
}
