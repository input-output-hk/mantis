package io.iohk.ethereum.consensus.pow.validators

import akka.util.ByteString

import io.iohk.ethereum.consensus.GetBlockHeaderByHash
import io.iohk.ethereum.consensus.GetNBlocksBack
import io.iohk.ethereum.consensus.Protocol
import io.iohk.ethereum.consensus.validators.BlockHeaderValidator
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator
import io.iohk.ethereum.consensus.validators.std.StdSignedTransactionValidator
import io.iohk.ethereum.consensus.validators.std.StdValidators
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.ledger.BlockExecutionError
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.BlockExecutionSuccess
import io.iohk.ethereum.utils.BlockchainConfig

trait ValidatorsExecutor extends Validators {
  def ommersValidator: OmmersValidator

  def validateBlockBeforeExecution(
      block: Block,
      getBlockHeaderByHash: GetBlockHeaderByHash,
      getNBlocksBack: GetNBlocksBack
  )(implicit
      blockchainConfig: BlockchainConfig
  ): Either[BlockExecutionError.ValidationBeforeExecError, BlockExecutionSuccess] =
    ValidatorsExecutor.validateBlockBeforeExecution(
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
  )(implicit
      blockchainConfig: BlockchainConfig
  ): Either[BlockExecutionError, BlockExecutionSuccess] =
    ValidatorsExecutor.validateBlockAfterExecution(
      self = this,
      block = block,
      stateRootHash = stateRootHash,
      receipts = receipts,
      gasUsed = gasUsed
    )
}

object ValidatorsExecutor {
  def apply(protocol: Protocol): ValidatorsExecutor = {
    val blockHeaderValidator: BlockHeaderValidator = protocol match {
      case Protocol.MockedPow     => MockedPowBlockHeaderValidator
      case Protocol.PoW           => PoWBlockHeaderValidator
      case Protocol.RestrictedPoW => RestrictedEthashBlockHeaderValidator
    }

    new StdValidatorsExecutor(
      StdBlockValidator,
      blockHeaderValidator,
      StdSignedTransactionValidator,
      new StdOmmersValidator(blockHeaderValidator)
    )
  }

  // Created only for testing purposes, shouldn't be used in production code.
  // Connected with: https://github.com/ethereum/tests/issues/480
  def apply(blockHeaderValidator: BlockHeaderValidator): ValidatorsExecutor =
    new StdValidatorsExecutor(
      StdBlockValidator,
      blockHeaderValidator,
      StdSignedTransactionValidator,
      new StdOmmersValidator(blockHeaderValidator)
    )

  def validateBlockBeforeExecution(
      self: ValidatorsExecutor,
      block: Block,
      getBlockHeaderByHash: GetBlockHeaderByHash,
      getNBlocksBack: GetNBlocksBack
  )(implicit
      blockchainConfig: BlockchainConfig
  ): Either[BlockExecutionError.ValidationBeforeExecError, BlockExecutionSuccess] = {

    val header = block.header
    val body = block.body

    val result = for {
      _ <- self.blockHeaderValidator.validate(header, getBlockHeaderByHash)
      _ <- self.blockValidator.validateHeaderAndBody(header, body)
      _ <- self.ommersValidator.validate(
        header.parentHash,
        header.number,
        body.uncleNodesList,
        getBlockHeaderByHash,
        getNBlocksBack
      )
    } yield BlockExecutionSuccess

    result.left.map(ValidationBeforeExecError)
  }

  def validateBlockAfterExecution(
      self: ValidatorsExecutor,
      block: Block,
      stateRootHash: ByteString,
      receipts: Seq[Receipt],
      gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess] =
    StdValidators.validateBlockAfterExecution(
      self = self,
      block = block,
      stateRootHash = stateRootHash,
      receipts = receipts,
      gasUsed = gasUsed
    )
}
