package io.iohk.ethereum.blockchain.sync.fast

import cats.data.NonEmptyList

import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.utils.Logger

trait FastSyncBranchResolver {

  import FastSyncBranchResolver._

  protected def blockchain: Blockchain
  protected def blockchainReader: BlockchainReader

  // TODO [ETCM-676] move to [[Blockchain]] and make sure it's atomic
  def discardBlocksAfter(lastValidBlock: BigInt): Unit =
    discardBlocks(lastValidBlock, blockchainReader.getBestBlockNumber())

  // TODO [ETCM-676] move to [[Blockchain]] and make sure it's atomic
  private def discardBlocks(fromBlock: BigInt, toBlock: BigInt): Unit = {
    val blocksToBeRemoved = childOf(fromBlock).to(toBlock).reverse.toList
    blocksToBeRemoved.foreach { toBeRemoved =>
      blockchainReader
        .getBlockHeaderByNumber(toBeRemoved)
        .foreach(header => blockchain.removeBlock(header.hash))
    }
  }

}

object FastSyncBranchResolver {

  /** Stores the current search state for binary search.
    * Meaning we know the first common block lies between minBlockNumber and maxBlockNumber.
    */
  final case class SearchState(minBlockNumber: BigInt, maxBlockNumber: BigInt, masterPeer: Peer)

  def parentOf(blockHeaderNumber: BigInt): BigInt = blockHeaderNumber - 1
  def childOf(blockHeaderNumber: BigInt): BigInt = blockHeaderNumber + 1
}

/** Attempt to find last common block within recent blocks by looking for a parent/child
  * relationship between our block headers and remote peer's block headers.
  */
class RecentBlocksSearch(blockchainReader: BlockchainReader) {

  /** Find the highest common block by trying to find a block so that our block n is the parent of remote candidate block n + 1
    */
  def getHighestCommonBlock(
      candidateHeaders: Seq[BlockHeader],
      bestBlockNumber: BigInt
  ): Option[BigInt] = {
    def isParent(potentialParent: BigInt, childCandidate: BlockHeader): Boolean =
      blockchainReader.getBlockHeaderByNumber(potentialParent).exists(_.isParentOf(childCandidate))
    NonEmptyList.fromList(candidateHeaders.reverse.toList).flatMap { remoteHeaders =>
      val blocksToBeCompared = bestBlockNumber.until(bestBlockNumber - remoteHeaders.size).by(-1).toList
      remoteHeaders.toList
        .zip(blocksToBeCompared)
        .collectFirst {
          case (childCandidate, parent) if isParent(parent, childCandidate) => parent
        }
    }
  }

}

object BinarySearchSupport extends Logger {
  import FastSyncBranchResolver._

  sealed trait BinarySearchResult
  final case class BinarySearchCompleted(highestCommonBlockNumber: BigInt) extends BinarySearchResult
  final case class ContinueBinarySearch(searchState: SearchState) extends BinarySearchResult
  case object NoCommonBlock extends BinarySearchResult

  /** Returns the block number in the middle between min and max.
    * If there is no middle, it will return the lower value.
    *
    * E.g. calling this method with min = 3 and max = 6 will return 4
    */
  def middleBlockNumber(min: BigInt, max: BigInt): BigInt = (min + max) / 2

  def blockHeaderNumberToRequest(min: BigInt, max: BigInt): BigInt =
    childOf(middleBlockNumber(min, max))

  def validateBlockHeaders(
      parentBlockHeader: BlockHeader,
      childBlockHeader: BlockHeader,
      searchState: SearchState
  ): BinarySearchResult = {
    val childNum = childBlockHeader.number
    val parentNum = parentBlockHeader.number
    val min = searchState.minBlockNumber
    val max = searchState.maxBlockNumber

    log.debug(
      "Validating block headers (binary search) for parentBlockHeader {}, remote childBlockHeader {} and search state {}",
      parentBlockHeader.number,
      childBlockHeader.number,
      searchState
    )

    if (parentBlockHeader.isParentOf(childBlockHeader)) { // chains are still aligned but there might be an even better block
      if (parentNum == max) BinarySearchCompleted(parentNum)
      else if (parentNum == min && childNum == max) ContinueBinarySearch(searchState.copy(minBlockNumber = childNum))
      else ContinueBinarySearch(searchState.copy(minBlockNumber = parentNum))
    } else { // no parent/child -> chains have diverged before parent block
      if (min == 1 && max <= 2) NoCommonBlock
      else if (min == max) BinarySearchCompleted(parentOf(parentNum))
      else ContinueBinarySearch(searchState.copy(maxBlockNumber = parentOf(parentNum).max(1)))
    }
  }

}
