package io.iohk.ethereum.consensus.validators.std

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex

import io.iohk.ethereum.consensus.GetBlockHeaderByHash
import io.iohk.ethereum.consensus.GetNBlocksBack
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.ledger.BlockExecutionError
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationAfterExecError
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.BlockExecutionSuccess
import io.iohk.ethereum.utils.BlockchainConfig

/** Implements validators that adhere to the original [[io.iohk.ethereum.consensus.validators.Validators Validators]]
  * interface.
  *
  * @see [[io.iohk.ethereum.consensus.pow.validators.StdValidatorsExecutor StdEthashValidators]]
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
  )(implicit blockchainConfig: BlockchainConfig): Either[ValidationBeforeExecError, BlockExecutionSuccess] =
    StdValidators.validateBlockBeforeExecution(
      self = this,
      block = block,
      getBlockHeaderByHash = getBlockHeaderByHash,
      getNBlocksBack = getNBlocksBack
    )

  def validateBlockAfterExecution(
      block: Block,
      stateRootHash: ByteString,
      receipts: Seq[Receipt],
      gasUsed: BigInt
  )(implicit blockchainConfig: BlockchainConfig): Either[BlockExecutionError, BlockExecutionSuccess] =
    StdValidators.validateBlockAfterExecution(
      self = this,
      block = block,
      stateRootHash = stateRootHash,
      receipts = receipts,
      gasUsed = gasUsed
    )
}

object StdValidators {
  def validateBlockBeforeExecution(
      self: Validators,
      block: Block,
      getBlockHeaderByHash: GetBlockHeaderByHash,
      getNBlocksBack: GetNBlocksBack
  )(implicit blockchainConfig: BlockchainConfig): Either[ValidationBeforeExecError, BlockExecutionSuccess] = {

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
        case _         => Right(BlockExecutionSuccess)
      }
    }
  }
}
