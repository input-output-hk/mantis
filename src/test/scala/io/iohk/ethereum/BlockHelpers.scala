package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.domain.Block

object BlockHelpers {

  def generateChain(amount: Int, parent: Block): Seq[Block] = {
    (1 to amount).foldLeft[Seq[Block]](Nil){ case (acc, _) =>
      val baseBlock = Fixtures.Blocks.ValidBlock.block

      val parentHeader = acc.lastOption.getOrElse(parent)
      val blockHeader = baseBlock.header.copy(
        number = parentHeader.number + 1,
        parentHash = parentHeader.hash,
        // Random nonce used for having our blocks be different
        nonce = ByteString(Math.random().toString)
      )
      val block = baseBlock.copy(header = blockHeader)

      acc :+ block
    }
  }

}
