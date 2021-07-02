package io.iohk.ethereum.consensus

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy

package object blocks {
  case class PendingBlock(block: Block, receipts: Seq[Receipt])
  case class PendingBlockAndState(pendingBlock: PendingBlock, worldState: InMemoryWorldStateProxy)
}
