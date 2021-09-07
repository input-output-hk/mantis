package io.iohk.ethereum.blockchain.sync.regular

import akka.NotUsed
import akka.stream.scaladsl.Flow
import akka.util.ByteString

import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.collection.immutable.Queue

import io.iohk.ethereum.blockchain.sync.regular.BranchBuffer._
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.branch.BestBranch
import io.iohk.ethereum.domain.branch.Branch

/** Naive & temporary branch buffer implementation with some serious limitations:
  * - it is not able to store multiple competing branches at the same time.
  * - it will only find branches from the best branch tip.
  *
  * @param byParent in-memory buffer of blocks retrievable by parentHash. Only one block per parent is kept, last received wins.
  * @param branchFound branch found from given best branch, as a list of blocks. They are removed from the buffer.
  */
case class BranchBuffer(byParent: Map[Hash, Block] = Map.empty, branchFound: Queue[Block] = Queue.empty) {
  def handle(trunk: Branch, block: Block): BranchBuffer =
    copy(byParent = byParent + (block.parentHash -> block), branchFound = Queue.empty)
      .branchFrom(trunk match {
        case BestBranch(tipBlockHash, _) => tipBlockHash
        case _                           => ByteString.empty
      })

  @tailrec
  private def branchFrom(hash: Hash): BranchBuffer =
    byParent.get(hash) match {
      case None        => this
      case Some(block) => copy(byParent.removed(hash), branchFound = branchFound :+ block).branchFrom(block.hash)
    }
}

object BranchBuffer {
  type Hash = ByteString

  def flow(blockchainReader: BlockchainReader): Flow[Block, NonEmptyList[Block], NotUsed] =
    Flow[Block]
      .scan(BranchBuffer()) { case (buffer, block) => buffer.handle(blockchainReader.getBestBranch(), block) }
      .collect { case BranchBuffer(_, head +: tail) => NonEmptyList(head, tail.toList) }
}
