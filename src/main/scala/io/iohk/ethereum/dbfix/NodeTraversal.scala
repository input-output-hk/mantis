package io.iohk.ethereum.dbfix

import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.mpt.MerklePatriciaTrie.{MissingNodeException, getChild, getNextNode, getRootNode}
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, LeafNode, MptNode}
import io.iohk.ethereum.mpt.MerklePatriciaTrie.nodeDec
import io.iohk.ethereum.utils.Logger
import org.spongycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}


class NodeTraversal(blockchain: BlockchainImpl) extends Logger {

  private val bestBlock = blockchain.getBestBlock()
  private val stateRoot = bestBlock.header.stateRoot
  private val blockNumber = bestBlock.header.number

  log.info(s"Starting MPT traversal for block number $blockNumber with state root hash ${Hex.toHexString(stateRoot.toArray)}")

  private val nodeStorage = blockchain.nodesKeyValueStorageFor(Some(blockNumber))
  private val rootNode = getRootNode(stateRoot.toArray, nodeStorage)

  private var nodesTraversed: Int = 0
  private var knownNodes: Int = 1
  private var missingNodes: Int = 0
  private var lastProgUpd: Long = System.currentTimeMillis()

  traverse(rootNode)
  lastProgUpd = 0
  logProgress()

  private def traverse(node: MptNode): Unit = {
    logProgress()
    nodesTraversed += 1

    node match {
      case l: LeafNode =>

      case b: BranchNode =>
        knownNodes += b.children.count(_.isDefined)

        for (i <- b.children.indices) {
          val child = Try(getChild(b, i, nodeStorage)) match {
            case Success(n) => n
            case Failure(ex: MissingNodeException) =>
              log.info(s"Found missing node: ${Hex.toHexString(ex.hash.toArray)}")
              missingNodes += 1
              None
            case Failure(ex) => throw ex
          }
          child.foreach(traverse)
        }

      case e: ExtensionNode =>
        knownNodes += 1

        Try(getNextNode(e, nodeStorage)) match {
          case Success(n) => traverse(n)
          case Failure(ex: MissingNodeException) =>
            log.info(s"Found missing node: ${Hex.toHexString(ex.hash.toArray)}")
            missingNodes += 1
          case Failure(ex) => throw ex
        }
    }
  }

  private def logProgress(): Unit = {
    val now = System.currentTimeMillis()
    val dt = now - lastProgUpd
    if (dt > 1000) {
      log.info(s"Traversed $nodesTraversed/$knownNodes. Missing nodes: $missingNodes")
      lastProgUpd = now
    }
  }
}
