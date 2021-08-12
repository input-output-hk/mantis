package io.iohk.ethereum.ledger

import cats.data.NonEmptyList

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.utils.Logger

class BranchResolution(blockchainReader: BlockchainReader) extends Logger {

  def resolveBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult =
    if (!doHeadersFormChain(headers)) {
      InvalidBranch
    } else {
      val knownParentOrGenesis = blockchainReader
        .isInChain(
          blockchainReader.getBestBranch(),
          headers.head.parentHash
        ) || headers.head.hash == blockchainReader.genesisHeader.hash

      if (!knownParentOrGenesis)
        UnknownBranch
      else
        compareBranch(headers)
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

    val maybeParentWeight: Option[Either[String, ChainWeight]] =
      oldBlocks.headOption
        .map(_.header)
        .orElse(newHeaders.headOption)
        .map { header =>
          blockchainReader
            .getChainWeightByHash(header.parentHash)
            .toRight(s"ChainWeight for ${header.idTag} not found when resolving branch: $newHeaders")
        }

    maybeParentWeight match {
      case Some(Right(parentWeight)) =>
        val oldWeight = oldBlocks.foldLeft(parentWeight)((w, b) => w.increase(b.header))
        val newWeight = newHeaders.foldLeft(parentWeight)((w, h) => w.increase(h))

        if (newWeight > oldWeight)
          NewBetterBranch(oldBlocks)
        else
          NoChainSwitch

      case Some(Left(err)) =>
        log.error(err)
        NoChainSwitch

      case None =>
        // after removing common prefix both 'new' and 'old` were empty
        log.warn("Attempted to compare identical branches")
        NoChainSwitch
    }
  }

  private def getTopBlocksFromNumber(from: BigInt): List[Block] = {
    val bestBranch = blockchainReader.getBestBranch()
    (from to blockchainReader.getBestBlockNumber())
      .flatMap(nb => blockchainReader.getBlockByNumber(bestBranch, nb))
      .toList
  }
}

sealed trait BranchResolutionResult

case class NewBetterBranch(oldBranch: Seq[Block]) extends BranchResolutionResult

case object NoChainSwitch extends BranchResolutionResult

case object UnknownBranch extends BranchResolutionResult

case object InvalidBranch extends BranchResolutionResult
