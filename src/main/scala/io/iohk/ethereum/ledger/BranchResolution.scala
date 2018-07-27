package io.iohk.ethereum.ledger

import io.iohk.ethereum.domain.{ Block, BlockHeader, Blockchain }

class BranchResolution(blockchain: Blockchain) {

  def doHeadersFormChain(headers: Seq[BlockHeader]): Boolean =
    if (headers.length > 1)
      headers.zip(headers.tail).forall {
        case (parent, child) =>
          parent.hash == child.parentHash && parent.number + 1 == child.number
      }
    else
      headers.nonEmpty

  private def getBlocksForHeaders(headers: Seq[BlockHeader]): List[Block] = headers match {
    case Seq(h, tail @ _*) =>
      blockchain.getBlockByNumber(h.number).map(_ :: getBlocksForHeaders(tail)).getOrElse(Nil)
    case Seq() =>
      Nil
  }

  // find blocks with same numbers in the current chain, removing any common prefix
  def removeCommonPrefix(headers: Seq[BlockHeader]): BranchResolutionResult = {
    val (oldBranch, _) = getBlocksForHeaders(headers).zip(headers)
      .dropWhile{ case (oldBlock, newHeader) => oldBlock.header == newHeader }.unzip
    val newHeaders = headers.dropWhile(h => oldBranch.headOption.exists(_.header.number > h.number))

    val currentBranchDifficulty = oldBranch.map(_.header.difficulty).sum
    val newBranchDifficulty = newHeaders.map(_.difficulty).sum

    if (currentBranchDifficulty < newBranchDifficulty) {
      NewBetterBranch(oldBranch)
    } else {
      NoChainSwitch
    }
  }
}

sealed trait BranchResolutionResult
case class  NewBetterBranch(oldBranch: Seq[Block]) extends BranchResolutionResult
case object NoChainSwitch extends BranchResolutionResult
case object UnknownBranch extends BranchResolutionResult
case object InvalidBranch extends BranchResolutionResult
