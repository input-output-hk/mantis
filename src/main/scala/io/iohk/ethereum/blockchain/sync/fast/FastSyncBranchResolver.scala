package io.iohk.ethereum.blockchain.sync.fast

import io.iohk.ethereum.domain.Blockchain

import cats.effect.Sync
import cats.Applicative
import cats.implicits._
import io.iohk.ethereum.domain.BlockHeader
import cats.data.NonEmptyList

import com.typesafe.scalalogging
import org.slf4j.LoggerFactory
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.network.Peer

trait FastSyncBranchResolver {

  import FastSyncBranchResolver._

  protected def blockchain: Blockchain

  // TODO [ETCM-676] move to [[Blockchain]] and make sure it's atomic
  def discardBlocksAfter(lastValidBlock: BigInt): Unit =
    discardBlocks(lastValidBlock, blockchain.getBestBlockNumber())

  // TODO [ETCM-676] move to [[Blockchain]] and make sure it's atomic
  private def discardBlocks(fromBlock: BigInt, toBlock: BigInt): Unit = {
    val blocksToBeRemoved = childOf(fromBlock).to(toBlock).reverse.toList
    blocksToBeRemoved.foreach { toBeRemoved =>
      blockchain
        .getBlockHeaderByNumber(toBeRemoved)
        .foreach(header => blockchain.removeBlock(header.hash, withState = false))
    }
  }

}

object FastSyncBranchResolver {

  /**
    * Stores the current search state for binary search.
    * Meaning we know the first common block lies between minBlockNumber and maxBlockNumber.
    */
  final case class SearchState(minBlockNumber: BigInt, maxBlockNumber: BigInt, masterPeer: Peer)

  def parentOf(blockHeaderNumber: BigInt): BigInt = blockHeaderNumber - 1
  def childOf(blockHeaderNumber: BigInt): BigInt = blockHeaderNumber + 1
}

/**
  * Attempt to find last common block within recent blocks by looking for a parent/child
  * relationship between our block headers and remote peer's block headers.
  */
class RecentBlocksSearch(blockchain: Blockchain) {

  /**
    * Find the highest common block by trying to find a block so that our block n is the parent of remote candidate block n + 1
    */
  def getHighestCommonBlock(
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

object BinarySearchSupport extends Logger {
  import FastSyncBranchResolver._

  sealed trait BinarySearchResult
  final case class BinarySearchCompleted(highestCommonBlockNumber: BigInt) extends BinarySearchResult
  final case class ContinueBinarySearch(searchState: SearchState) extends BinarySearchResult
  case object NoCommonBlock extends BinarySearchResult

  /**
    * Returns the block number in the middle between min and max.
    * If there is no middle, it will return the lower value.
    *
    * E.g. calling this method with min = 3 and max = 6 will return 4
    */
  def middleBlockNumber(min: BigInt, max: BigInt): BigInt = (min + max) / 2

  def blockHeaderNumberToRequest(min: BigInt, max: BigInt): BigInt =
    childOf(middleBlockNumber(min, max))

  def validateBlockHeadersFoo(
      parentBlockHeader: BlockHeader,
      childBlockHeader: BlockHeader,
      searchState: SearchState
  ): BinarySearchResult = {
    log.debug(s"$searchState")
    if (parentBlockHeader.isParentOf(childBlockHeader)) {
      if (parentBlockHeader.number == searchState.minBlockNumber)
        BinarySearchCompleted(parentBlockHeader.number)
      else ContinueBinarySearch(searchState.copy(minBlockNumber = parentBlockHeader.number))
    } else if (parentBlockHeader.number == searchState.minBlockNumber && searchState.minBlockNumber == 0)
      NoCommonBlock
    else if (parentBlockHeader.number == searchState.minBlockNumber)
      BinarySearchCompleted(parentOf(parentBlockHeader.number))
    else
      ContinueBinarySearch(searchState.copy(maxBlockNumber = parentBlockHeader.number))
  }

  def validateBlockHeaders(
      parentBlockHeader: BlockHeader,
      childBlockHeader: BlockHeader,
      searchState: SearchState
  ): BinarySearchResult = {
    val childNum = childBlockHeader.number
    val parentNum = parentBlockHeader.number
    val min = searchState.minBlockNumber
    val max = searchState.maxBlockNumber

    log.debug(s"$searchState")

    if (parentBlockHeader.isParentOf(childBlockHeader)) { // chains are still aligned but there might be an even better block
      if (parentNum == max) BinarySearchCompleted(parentNum)
      else if (parentNum == min && childNum == max) ContinueBinarySearch(searchState.copy(minBlockNumber = childNum))
      else ContinueBinarySearch(searchState.copy(minBlockNumber = parentNum))
    } else { // no parent/child -> chains have diverged before parent block
      if (min == 1 && max == 2) NoCommonBlock
      else if (min == max) BinarySearchCompleted(parentOf(parentNum))
      // else if (parentNum == max) BinarySearchCompleted(parentNum)
      else ContinueBinarySearch(searchState.copy(maxBlockNumber = parentOf(parentNum)))
    }
  }

}
