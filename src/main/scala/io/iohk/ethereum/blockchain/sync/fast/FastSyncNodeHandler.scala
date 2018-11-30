package io.iohk.ethereum.blockchain.sync.fast

import akka.event.LoggingAdapter
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.blockchain.sync.fast.FastSync._
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{ Account, Blockchain }
import io.iohk.ethereum.mpt.{ BranchNode, ExtensionNode, HashNode, LeafNode, MptNode }
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._
import io.iohk.ethereum.network.p2p.messages.PV63.{ AccountImplicits, NodeData }
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.duration.FiniteDuration
import scala.util.{ Failure, Success, Try }

trait FastSyncNodeHandler {

  type Nodes = Seq[HashType]

  def syncConfig: SyncConfig
  def blockchain: Blockchain
  def log: LoggingAdapter

  def handleNodeData(
    peer: Peer,
    requestedHashes: Nodes,
    nodeData: NodeData,
    downloadedNodesCount: Int,
    targetNumber: BigInt,
    blacklist: (BlackListId, FiniteDuration, String) => Unit
  ): (Nodes, Int, Int) = {
    val nodeValues = nodeData.values
    if (nodeValues.isEmpty) {
      val hashes = requestedHashes.map(h => Hex.toHexString(h.v.toArray[Byte]))
      log.debug("Got empty mpt node response for known hashes, switching to blockchain only: {}", hashes)
      blacklist(peer.id, syncConfig.blacklistDuration, "empty mpt node response for known hashes")
      (requestedHashes, downloadedNodesCount, downloadedNodesCount)
    } else {
      val receivedHashes = nodeValues.map(v => ByteString(kec256(v.toArray[Byte])))
      val remainingHashes = requestedHashes.filterNot(h => receivedHashes.contains(h.v))

      val pendingNodes = collectPendingNodes(nodeData, requestedHashes, receivedHashes, targetNumber)
      val downloadedNodes = downloadedNodesCount + nodeValues.size
      val newKnownNodes = downloadedNodes + pendingNodes.size

      val nodes = remainingHashes ++ pendingNodes
      log.debug("Node data count: pending: {}, downloaded: {}, total: {}", nodes.size, downloadedNodes, newKnownNodes)
      (nodes, downloadedNodes, newKnownNodes)
    }
  }

  private def collectPendingNodes(nodeData: NodeData, requested: Nodes, received: Seq[ByteString], targetNumber: BigInt): Nodes = {
    val nodeValues = nodeData.values
    (nodeValues.indices zip received) flatMap { case (idx, valueHash) =>
      requested.find(_.v == valueHash) map {
        case StateMptNodeHash(_) =>
          tryToDecodeNodeData(nodeData, idx, targetNumber, handleMptNode)

        case ContractStorageMptNodeHash(_) | StorageRootHash(_) =>
          tryToDecodeNodeData(nodeData, idx, targetNumber, handleContractMptNode)

        case EvmCodeHash(hash) =>
          blockchain.save(hash, nodeValues(idx))
          Nil
      }
    }
  }.flatten

  private def tryToDecodeNodeData(nodeData: NodeData, idx: Int, targetNumber: BigInt, func: (MptNode, BigInt) => Nodes): Nodes = {
    // getMptNode throws RLPException
    Try(nodeData.getMptNode(idx)) match {
      case Success(node) =>
        func(node, targetNumber)

      case Failure(msg)  =>
        log.warning("Cannot decode {} due to: {}", nodeData, msg.getMessage)
        Nil
    }
  }

  private def handleMptNode(mptNode: MptNode, targetBlock: BigInt): Nodes = mptNode match {
    case node: LeafNode =>
      saveFastSyncNode(node, targetBlock)
      tryToDecodeLeafNode(node)

    case node: BranchNode =>
      saveFastSyncNode(node, targetBlock)
      node.children.collect { case HashNode(childHash) => StateMptNodeHash(ByteString(childHash)) }

    case node: ExtensionNode =>
      saveFastSyncNode(node, targetBlock)
      node.next match {
        case HashNode(hashNode) => Seq(StateMptNodeHash(ByteString(hashNode)))
        case _                  => Nil
      }

    case _ => Nil
  }

  private def tryToDecodeLeafNode(node: LeafNode): Nodes = {
    import AccountImplicits._
    // If this fails it means that we have LeafNode which is part of MPT that do not stores account
    // We verify if node is part of the tree by checking its hash before we call this method in collectPendingNodes
    Try(node.value.toArray[Byte].toAccount) match {
      case Success(Account(_, _, storageRoot, codeHash)) =>
        val evm = if (codeHash == Account.EmptyCodeHash) Nil else Seq(EvmCodeHash(codeHash))
        val storage = if (storageRoot == Account.EmptyStorageRootHash) Nil else Seq(StorageRootHash(storageRoot))
        evm ++ storage

      case Failure(e) =>
        log.debug("Leaf node without account, error while trying to decode account: {}", e.getMessage)
        Nil
    }
  }

  private def handleContractMptNode(mptNode: MptNode, targetBlock: BigInt): Nodes = {
    mptNode match {
      case node: LeafNode =>
        saveFastSyncNode(node, targetBlock)
        Nil

      case node: BranchNode =>
        saveFastSyncNode(node, targetBlock)
        node.children.collect { case HashNode(childHash) => ContractStorageMptNodeHash(ByteString(childHash)) }

      case node: ExtensionNode =>
        saveFastSyncNode(node, targetBlock)
        node.next match {
          case HashNode(hashNode) => Seq(ContractStorageMptNodeHash(ByteString(hashNode)))
          case _                  => Nil
        }

      case _ => Nil
    }
  }

  private def saveFastSyncNode(node: MptNode, targetBlock: BigInt): Unit = {
    blockchain.saveNode(ByteString(node.hash), node.toBytes, targetBlock)
  }
}
