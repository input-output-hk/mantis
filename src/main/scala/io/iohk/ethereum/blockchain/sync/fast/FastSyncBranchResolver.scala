package io.iohk.ethereum.blockchain.sync.fast

import io.iohk.ethereum.domain.Blockchain

import cats.effect.Sync
import cats.Applicative
import cats.implicits._
import io.iohk.ethereum.domain.BlockHeader
import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.fast.FastSyncBranchResolverActor.SearchState

trait FastSyncBranchResolver {

  protected def blockchain: Blockchain

  def parentOf(blockHeaderNumber: BigInt): BigInt = blockHeaderNumber - 1

  def childOf(blockHeaderNumber: BigInt): BigInt = blockHeaderNumber + 1

  def discardBlocksAfter(lastValidBlock: BigInt): Unit =
    discardBlocks(lastValidBlock, blockchain.getBestBlockNumber())

  def discardBlocks(fromBlock: BigInt, toBlock: BigInt): Unit = {
    val blocksToBeRemoved = childOf(fromBlock).to(toBlock).reverse.toList
    blocksToBeRemoved.foreach { toBeRemoved =>
      blockchain
        .getBlockHeaderByNumber(toBeRemoved)
        .foreach(header => blockchain.removeBlock(header.hash, withState = false))
    }
  }

}

/**
  * Attempt to find last common block within recent blocks by looking for a parent/child
  * relationship between our block headers and remote peer's block headers.
  */
trait RecentBlocksSearchSupport {
  self: FastSyncBranchResolver =>

  protected def recentHeadersSize: Int

  /**
    * Find the first common block by trying to find a block so that our block n is the parent of remote block n + 1
    */
  def getFirstCommonBlock(
      candidateHeaders: Seq[BlockHeader],
      bestBlockNumber: BigInt
  ): Option[BigInt] = {
    NonEmptyList.fromList(candidateHeaders.reverse.toList).flatMap { remoteHeaders =>
      val blocksToBeCompared = bestBlockNumber.until(bestBlockNumber - remoteHeaders.size).by(-1).toList
      candidateHeaders.reverse
        .zip(blocksToBeCompared)
        .find { case (childCandidate, parent) =>
          blockchain.getBlockHeaderByNumber(parent).exists { _.isParentOf(childCandidate) }
        }
        .map(_._2)
    }
  }

}

trait BinarySearchSupport {
  self: FastSyncBranchResolver =>

  import BinarySearchSupport._

  // TODO simplify this O_o
  protected def validateBlockHeaders(
      parentBlockHeader: BlockHeader,
      childBlockHeader: BlockHeader,
      searchState: SearchState
  ): BinarySearchResult = {
    if (parentBlockHeader.isParentOf(childBlockHeader)) {
      if (parentBlockHeader.number == searchState.minBlockNumber)
        BinarySearchCompleted(parentBlockHeader.number)
      else ContinueBinarySearch(searchState.copy(minBlockNumber = childOf(parentBlockHeader.number)))
    } else if (parentBlockHeader.number == searchState.minBlockNumber)
      BinarySearchCompleted(parentOf(parentBlockHeader.number)) // TODO not really
    else
      ContinueBinarySearch(searchState.copy(maxBlockNumber = childOf(parentBlockHeader.number)))
  }

}

object BinarySearchSupport {

  sealed trait BinarySearchResult
  final case class BinarySearchCompleted(firstCommonBlockNumber: BigInt) extends BinarySearchResult
  final case class ContinueBinarySearch(searchState: SearchState) extends BinarySearchResult

}
