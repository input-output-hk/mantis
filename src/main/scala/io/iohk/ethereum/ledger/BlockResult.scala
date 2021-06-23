package io.iohk.ethereum.ledger

import io.iohk.ethereum.domain.Receipt

case class BlockResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt = 0, receipts: Seq[Receipt] = Nil)
