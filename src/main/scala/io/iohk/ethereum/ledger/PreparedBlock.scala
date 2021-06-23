package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.domain.Block

case class PreparedBlock(
    block: Block,
    blockResult: BlockResult,
    stateRootHash: ByteString,
    updatedWorld: InMemoryWorldStateProxy
)
