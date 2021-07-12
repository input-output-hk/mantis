package io.iohk.ethereum.blockchain.sync.regular

import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.{BlockImportType, ImportFn, ResolvingBranch}
import io.iohk.ethereum.domain.Block

//not used atm, a part of the future ExecutionSync
class BlockImporterService() {

  private def importBlocks(blocks: NonEmptyList[Block], blockImportType: BlockImportType) = ???

  private def importBlock(block: Block) = ???

}
