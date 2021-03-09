package io.iohk.ethereum.consensus.validators

import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.BlockError
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.BlockValid
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.Receipt

trait BlockValidator {
  def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody): Either[BlockError, BlockValid]

  def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]): Either[BlockError, BlockValid]
}
