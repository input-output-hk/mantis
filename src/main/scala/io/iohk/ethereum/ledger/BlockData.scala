package io.iohk.ethereum.ledger

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.domain.ChainWeight

case class BlockData(block: Block, receipts: Seq[Receipt], weight: ChainWeight)
