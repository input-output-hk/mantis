package io.iohk.ethereum.consensus.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.{BlockError, BlockValid}
import io.iohk.ethereum.domain.{Block, BlockHeader, Receipt}
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

trait BlockValidator {
  def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody): Either[BlockError, BlockValid]

  def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]): Either[BlockError, BlockValid]

  def validateBlockAfterExecution(
    block: Block,
    stateRootHash: ByteString,
    receipts: Seq[Receipt],
    gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess] = ??? // FIXME implement
}
