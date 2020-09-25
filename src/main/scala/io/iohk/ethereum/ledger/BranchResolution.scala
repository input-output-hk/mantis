package io.iohk.ethereum.ledger

import cats.data.NonEmptyList
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain}

class BranchResolution(blockchain: Blockchain) {

  def resolveBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult = {
    if (!doHeadersFormChain(headers)) {
      InvalidBranch
    } else {
      val knownParentOrGenesis = blockchain
        .getBlockHeaderByHash(headers.head.parentHash)
        .isDefined || headers.head.hash == blockchain.genesisHeader.hash

      if (!knownParentOrGenesis)
        UnknownBranch
      else
        compareBranch(headers)
    }
  }

  private[ledger] def doHeadersFormChain(headers: NonEmptyList[BlockHeader]): Boolean =
    headers.toList.zip(headers.tail).forall { case (parent, child) =>
      parent.hash == child.parentHash && parent.number + 1 == child.number
    }

  private[ledger] def compareBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult = {
    val headersList = headers.toList
    val oldBlocksWithCommonPrefix = getTopBlocksFromNumber(headers.head.number)

    val commonPrefixLength = oldBlocksWithCommonPrefix
      .zip(headersList)
      .takeWhile { case (oldBlock, newHeader) => oldBlock.header == newHeader }
      .length

    val oldBlocks = oldBlocksWithCommonPrefix.drop(commonPrefixLength)
    val newHeaders = headersList.drop(commonPrefixLength)

    if (compareByCheckpoints(newHeaders, oldBlocks.map(_.header)))
      NewBetterBranch(oldBlocks)
    else
      NoChainSwitch
  }

  /**
    * @return true if newBranch is better than oldBranch
    */
  private def compareByCheckpoints(newBranch: Seq[BlockHeader], oldBranch: Seq[BlockHeader]): Boolean =
    (branchLatestCheckpoint(newBranch), branchLatestCheckpoint(oldBranch)) match {
      case (Some(newCheckpoint), Some(oldCheckpoint)) =>
        if (newCheckpoint.number == oldCheckpoint.number)
          compareByDifficulty(newBranch, oldBranch)
        else
          newCheckpoint.number > oldCheckpoint.number

      case (Some(_), None) =>
        true

      case (None, Some(_)) =>
        false

      case (None, None) =>
        compareByDifficulty(newBranch, oldBranch)
    }

  /**
    * @return true if newBranch is better than oldBranch
    */
  private def compareByDifficulty(newBranch: Seq[BlockHeader], oldBranch: Seq[BlockHeader]): Boolean = {
    val newDifficulty = newBranch.map(_.difficulty).sum
    val oldDifficulty = oldBranch.map(_.difficulty).sum
    newDifficulty > oldDifficulty
  }

  private def getTopBlocksFromNumber(from: BigInt): List[Block] =
    (from to blockchain.getBestBlockNumber())
      .flatMap(blockchain.getBlockByNumber)
      .toList

  private def branchLatestCheckpoint(headers: Seq[BlockHeader]): Option[BlockHeader] =
    headers.filter(_.hasCheckpoint) match {
      case Seq() => None
      case checkpoints => Some(checkpoints.maxBy(_.number))
    }
}

sealed trait BranchResolutionResult

case class NewBetterBranch(oldBranch: Seq[Block]) extends BranchResolutionResult

case object NoChainSwitch extends BranchResolutionResult

case object UnknownBranch extends BranchResolutionResult

case object InvalidBranch extends BranchResolutionResult
