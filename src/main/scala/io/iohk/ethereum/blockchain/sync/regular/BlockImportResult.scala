package io.iohk.ethereum.blockchain.sync.regular

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.ledger.BlockData
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException

sealed trait BlockImportResult

case class BlockImportedToTop(blockImportData: List[BlockData]) extends BlockImportResult

case object BlockEnqueued extends BlockImportResult

case object DuplicateBlock extends BlockImportResult

case class ChainReorganised(
    oldBranch: List[Block],
    newBranch: List[Block],
    weights: List[ChainWeight]
) extends BlockImportResult

case class BlockImportFailed(error: String) extends BlockImportResult

case class BlockImportFailedDueToMissingNode(reason: MissingNodeException) extends BlockImportResult

case object UnknownParent extends BlockImportResult
