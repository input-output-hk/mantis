package io.iohk.ethereum.consensus.validators.std

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.consensus.{GetBlockHeaderByHash, GetNBlocksBack}
import io.iohk.ethereum.domain.{Block, Receipt}
import io.iohk.ethereum.ledger.BlockExecutionError.{ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess}
import org.bouncycastle.util.encoders.Hex

/**
  * Implements validators that adhere to the original [[io.iohk.ethereum.consensus.validators.Validators Validators]]
  * interface.
  *
  * @see [[io.iohk.ethereum.consensus.ethash.validators.StdValidatorsExecutor StdEthashValidators]]
  *      for the PoW-specific counterpart.
  */
final class StdValidators(
    val blockValidator: BlockValidator,
    val blockHeaderValidator: BlockHeaderValidator,
    val signedTransactionValidator: SignedTransactionValidator
) extends Validators {

  def validateBlockBeforeExecution(
      block: Block,
      getBlockHeaderByHash: GetBlockHeaderByHash,
      getNBlocksBack: GetNBlocksBack
  ): Either[ValidationBeforeExecError, BlockExecutionSuccess] = {

    StdValidators.validateBlockBeforeExecution(
      self = this,
      block = block,
      getBlockHeaderByHash = getBlockHeaderByHash,
      getNBlocksBack = getNBlocksBack
    )
  }

  def validateBlockAfterExecution(
      block: Block,
      stateRootHash: ByteString,
      receipts: Seq[Receipt],
      gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess] = {

    StdValidators.validateBlockAfterExecution(
      self = this,
      block = block,
      stateRootHash = stateRootHash,
      receipts = receipts,
      gasUsed = gasUsed
    )
  }
}

object StdValidators {
  def validateBlockBeforeExecution(
      self: Validators,
      block: Block,
      getBlockHeaderByHash: GetBlockHeaderByHash,
      getNBlocksBack: GetNBlocksBack
  ): Either[ValidationBeforeExecError, BlockExecutionSuccess] = {

    val header = block.header
    val body = block.body

    val result = for {
      _ <- self.blockHeaderValidator.validate(header, getBlockHeaderByHash)
      _ <- self.blockValidator.validateHeaderAndBody(header, body)
    } yield BlockExecutionSuccess

    result.left.map(ValidationBeforeExecError)
  }

  def validateBlockAfterExecution(
      self: Validators,
      block: Block,
      stateRootHash: ByteString,
      receipts: Seq[Receipt],
      gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess] = {

    val header = block.header
    val blockAndReceiptsValidation = self.blockValidator.validateBlockAndReceipts(header, receipts)

    if (header.gasUsed != gasUsed)
      Left(ValidationAfterExecError(s"Block has invalid gas used, expected ${header.gasUsed} but got $gasUsed"))
    else if (header.stateRoot != stateRootHash)
      Left(ValidationAfterExecError(s"Block has invalid state root hash, expected ${Hex
        .toHexString(header.stateRoot.toArray)} but got ${Hex.toHexString(stateRootHash.toArray)}"))
    else {
      blockAndReceiptsValidation match {
        case Left(err) => Left(ValidationAfterExecError(err.toString))
        case _ => Right(BlockExecutionSuccess)
      }
    }
  }
}
