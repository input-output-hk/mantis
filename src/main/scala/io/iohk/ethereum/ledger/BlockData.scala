package io.iohk.ethereum.ledger

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.domain.Receipt

case class BlockData(block: Block, receipts: Seq[Receipt], weight: ChainWeight)
