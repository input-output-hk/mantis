package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.ledger.BlockQueue.{Leaf, QueuedBlock}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.Logger
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec
import scala.collection.mutable


object BlockQueue {
  case class QueuedBlock(block: Block, totalDifficulty: Option[BigInt])
  case class Leaf(hash: ByteString, totalDifficulty: BigInt)
}

class BlockQueue(blockchain: Blockchain, syncConfig: SyncConfig) extends Logger {

  private val blocks = mutable.Map[ByteString, QueuedBlock]()
  private val parentToChildren = mutable.Map[ByteString, Set[ByteString]]()

  /**
    * Enqueue a block for optional later inclusion into the blockchain
    *
    * @param block the block to be enqueued
    * @return if the newly enqueued block is part of a known branch (rooted somewhere on the main chain), return
    *         the leaf hash and its total difficulty, otherwise None
    */
  def enqueueBlock(block: Block, bestBlockNumber: BigInt = blockchain.getBestBlockNumber()): Option[Leaf] = {
    import block.header._

    cleanUp(bestBlockNumber)

    blocks.get(hash) match {

      case Some(_) =>
        log.debug(s"Block (${blockId(block)}) already in queue. ")
        None

      case None if isNumberOutOfRange(number, bestBlockNumber) =>
        log.debug(s"Block (${blockId(block)} is outside accepted range. Current best block number is: $bestBlockNumber")
        None

      case None =>
        val parentTd = blockchain.getTotalDifficultyByHash(parentHash)

        parentTd match {

          case Some(_) =>
            addBlock(block, parentTd)
            log.debug(s"Enqueued new block (${blockId(block)}) with parent on the main chain")
            updateTotalDifficulties(hash)

          case None =>
            addBlock(block, parentTd)
            findClosestChainedAncestor(block) match {
              case Some(ancestor) =>
                log.debug(s"Enqueued new block (${blockId(block)}) to a rooted sidechain")
                updateTotalDifficulties(ancestor)

              case None =>
                log.debug(s"Enqueued new block (${blockId(block)}) with unknown relation to the main chain")
                None
            }
        }
    }
  }

  def isQueued(hash: ByteString): Boolean =
    blocks.get(hash).isDefined


  /**
    * Removes a branch going from descendant block upwards to the oldest ancestor. Shared part of branch is not removed
    * @param descendant the youngest block to be removed
    * @return full branch from oldest ancestor to descendant, even if not all of it is removed
    */
  def removeBranch(descendant: ByteString): List[Block] = {

    def recur(hash: ByteString, childShared: Boolean): List[Block] = {
      blocks.get(hash) match {
        case Some(QueuedBlock(block, _)) =>
          import block.header.parentHash

          val isShared = childShared || parentToChildren.get(hash).exists(_.nonEmpty)
          if (!isShared) {
            val siblings = parentToChildren.get(parentHash)
            siblings.foreach(sbls => parentToChildren += parentHash -> (sbls - hash))
            blocks -= hash
          }

          block :: recur(parentHash, isShared)

        case None =>
          Nil
      }
    }

    recur(descendant, false).reverse
  }

  /**
    * Removes a whole subtree begining with the ancestor. To be used when ancestor fails to execute
    * @param ancestor hash of the ancestor block
    */
  def removeSubtree(ancestor: ByteString): Unit =
    blocks.get(ancestor).foreach { case QueuedBlock(block, _) =>
      val children = parentToChildren.getOrElse(ancestor, Set.empty)
      children.foreach(removeSubtree)
      blocks -= block.header.hash
      parentToChildren -= block.header.hash
    }

  /**
    * Removes stale blocks - too old or too young in relation the current best block number
    * @param bestBlockNumber - best block number of the main chain
    */
  private def cleanUp(bestBlockNumber: BigInt): Unit = {
    val staleHashes = blocks.values.collect {
      case QueuedBlock(b, _) if isNumberOutOfRange(b.header.number, bestBlockNumber) =>
        b.header.hash
    }

    blocks --= staleHashes
    parentToChildren --= staleHashes
  }

  /**
    * Updated total difficulties for a subtree.
    * @param ancestor An ancestor's hash that determines the subtree
    * @return Best leaf from the affected subtree
    */
  private def updateTotalDifficulties(ancestor: ByteString): Option[Leaf] = {
    blocks.get(ancestor).flatMap(_.totalDifficulty).map { td =>
      parentToChildren.get(ancestor) match {

        case Some(children) if children.nonEmpty =>
          val updatedChildren = children.flatMap(blocks.get)
            .map(qb => qb.copy(totalDifficulty = Some(td + qb.block.header.difficulty)))
          updatedChildren.foreach(qb => blocks += qb.block.header.hash -> qb)
          updatedChildren.flatMap(qb => updateTotalDifficulties(qb.block.header.hash)).maxBy(_.totalDifficulty)

        case _ =>
          Leaf(ancestor, td)
      }
    }
  }

  /**
    * Find a closest (youngest) chained ancestor. Chained means being part of a known chain, thus having total
    * difficulty defined
    *
    * @param descendant the block we start the search from
    * @return hash of the ancestor, if found
    */
  @tailrec
  private def findClosestChainedAncestor(descendant: Block): Option[ByteString] =
    blocks.get(descendant.header.parentHash) match {
      case Some(QueuedBlock(block, Some(_))) =>
        Some(block.header.hash)

      case Some(QueuedBlock(block, None)) =>
        findClosestChainedAncestor(block)

      case None =>
        None
    }

  private def addBlock(block: Block, parentTd: Option[BigInt]): Unit = {
    import block.header._

    val td = parentTd.map(_ + difficulty)
    blocks += hash -> QueuedBlock(block, td)

    val siblings = parentToChildren.getOrElse(parentHash, Set.empty)
    parentToChildren += parentHash -> (siblings + hash)
  }

  private def isNumberOutOfRange(blockNumber: BigInt, bestBlockNumber: BigInt): Boolean =
    blockNumber - bestBlockNumber > syncConfig.maxQueuedBlockNumberAhead ||
    bestBlockNumber - blockNumber > syncConfig.maxQueuedBlockNumberBehind

  private def blockId(block: Block): String =
    s"${block.header.number}: ${Hex.toHexString(block.header.hash.toArray)}"
}
