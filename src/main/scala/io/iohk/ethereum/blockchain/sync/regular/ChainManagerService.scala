package io.iohk.ethereum.blockchain.sync.regular

import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.BlockImportType
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.network.Peer
import monix.eval.Task

//not used atm, a part of the future ExecutionSync
object ChainManagerService {

  type ValidationError
  type Branch

  //saves blocks to the storage
  def processBlocksFromFetcher(peer: Peer): Task[Either[ValidationError, Branch]] = ???

  //saves blocks to the storage
  def processMinedBlocks(): Task[Either[ValidationError, Branch]] = ???

  private def resolvingMissingNode(blocksToRetry: NonEmptyList[Block], blockImportType: BlockImportType) = ???

  private def buildBranch() = ???

  private def resolveBranch() = ???

  private def discriminateBranch() = ???
}
