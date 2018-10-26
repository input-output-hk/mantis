package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorLogging
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.blockchain.sync.FastSync._
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

trait FastSyncNodeHandler { this: ActorLogging =>

  def syncConfig: SyncConfig
  def blockchain: Blockchain

  def handleNodeData(
    peer: Peer,
    requestedHashes: Seq[HashType],
    nodeData: NodeData,
    handlerState: FastSyncHandlerState,
    blacklist: (BlackListId, FiniteDuration, String) => Unit
  ): FastSyncHandlerState = {
    val nodeValues = nodeData.values
    if (nodeValues.isEmpty) {
      val hashes = requestedHashes.map(h => Hex.toHexString(h.v.toArray[Byte]))
      log.debug(s"Got empty mpt node response for known hashes switching to blockchain only: $hashes")
      blacklist(peer.id, syncConfig.blacklistDuration, "empty mpt node response for known hashes")
    }

    val receivedHashes = nodeValues.map(v => ByteString(kec256(v.toArray[Byte])))
    val remainingHashes = requestedHashes.filterNot(h => receivedHashes.contains(h.v))

    val pendingNodes = collectPendingNodes(nodeData, requestedHashes, receivedHashes, handlerState.syncState.targetBlock.number)
    val downloadedNodes = handlerState.syncState.downloadedNodesCount + nodeValues.size
    val newKnownNodes = downloadedNodes + pendingNodes.size

    handlerState.withNodeData(remainingHashes ++ pendingNodes, downloadedNodes, newKnownNodes)
  }

  private def collectPendingNodes(nodeData: NodeData, requested: Seq[HashType], received: Seq[ByteString], targetNumber: BigInt): Seq[HashType] = {
    val nodeValues = nodeData.values
    (nodeValues.indices zip received) flatMap { case (idx, valueHash) =>
      requested.filter(_.v == valueHash) flatMap {
        case _: StateMptNodeHash =>
          tryToDecodeNodeData(nodeData, idx, targetNumber, handleMptNode)

        case _: ContractStorageMptNodeHash | _: StorageRootHash =>
          tryToDecodeNodeData(nodeData, idx, targetNumber, handleContractMptNode)

        case EvmCodeHash(hash) =>
          blockchain.save(hash, nodeValues(idx))
          Nil
      }
    }
  }

  private def tryToDecodeNodeData(nodeData: NodeData, idx: Int, targetNumber: BigInt, func: (MptNode, BigInt) => Seq[HashType]): Seq[HashType] = {
    // getMptNode throws RLPException
    Try(nodeData.getMptNode(idx)) match {
      case Success(node) =>
        func(node, targetNumber)

      case Failure(msg)  =>
        log.warning(s"Cannot decode $nodeData due to: ${msg.getMessage}")
        Nil
    }
  }

  private def handleMptNode(mptNode: MptNode, targetBlock: BigInt): Seq[HashType] = mptNode match {
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

  private def tryToDecodeLeafNode(node: LeafNode): Seq[HashType] = {
    import AccountImplicits._
    // If this fails it means that we have LeafNode which is part of MPT that do not stores account
    // We verify if node is part of the tree by checking its hash before we call this method in collectPendingNodes
    Try(node.value.toArray[Byte].toAccount) match {
      case Success(Account(_, _, storageRoot, codeHash)) =>
        val evm = if (codeHash == Account.EmptyCodeHash) Nil else Seq(EvmCodeHash(codeHash))
        val storage = if (storageRoot == Account.EmptyStorageRootHash) Nil else Seq(StorageRootHash(storageRoot))
        evm ++ storage

      case Failure(e) =>
        log.debug(s"Leaf node without account, error while trying to decode account: ${e.getMessage}")
        Nil
    }
  }

  private def handleContractMptNode(mptNode: MptNode, targetBlock: BigInt): Seq[HashType] = {
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
    blockchain.saveFastSyncNode(ByteString(node.hash), node.toBytes, targetBlock)
  }
}
