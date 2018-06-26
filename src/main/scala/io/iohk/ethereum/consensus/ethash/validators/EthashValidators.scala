package io.iohk.ethereum.consensus
package ethash.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.consensus.validators.std.{StdBlockValidator, StdSignedTransactionValidator, StdValidators}
import io.iohk.ethereum.domain.{Block, Receipt}
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess}
import io.iohk.ethereum.utils.BlockchainConfig

trait EthashValidators extends Validators {
  def ommersValidator: OmmersValidator

  def validateBlockBeforeExecution(
    block: Block,
    getBlockHeaderByHash: GetBlockHeaderByHash,
    getNBlocksBack: GetNBlocksBack
  ): Either[BlockExecutionError.ValidationBeforeExecError, BlockExecutionSuccess] = {

    EthashValidators.validateBlockBeforeExecution(
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

    EthashValidators.validateBlockAfterExecution(
      self = this,
      block = block,
      stateRootHash = stateRootHash,
      receipts = receipts,
      gasUsed = gasUsed
    )
  }
}

object EthashValidators {
  def apply(blockchainConfig: BlockchainConfig): EthashValidators = {
    val blockHeaderValidator = new EthashBlockHeaderValidator(blockchainConfig)

    new StdEthashValidators(
      new StdBlockValidator(blockchainConfig.ethCompatibilityMode),
      blockHeaderValidator,
      new StdSignedTransactionValidator(blockchainConfig),
      new StdOmmersValidator(blockchainConfig, blockHeaderValidator)
    )
  }

  def validateBlockBeforeExecution(
    self: EthashValidators,
    block: Block,
    getBlockHeaderByHash: GetBlockHeaderByHash,
    getNBlocksBack: GetNBlocksBack
  ): Either[BlockExecutionError.ValidationBeforeExecError, BlockExecutionSuccess] = {

    val header = block.header
    val body = block.body

    val result = for {
      _ <- self.blockHeaderValidator.validate(header, getBlockHeaderByHash)
      _ <- self.blockValidator.validateHeaderAndBody(header, body)
      _ <- self.ommersValidator.validate(header.parentHash, header.number, body.uncleNodesList,
        getBlockHeaderByHash, getNBlocksBack)
    } yield BlockExecutionSuccess

    result.left.map(ValidationBeforeExecError)
  }

  def validateBlockAfterExecution(
    self: EthashValidators,
    block: Block,
    stateRootHash: ByteString,
    receipts: Seq[Receipt],
    gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess] = {

    StdValidators.validateBlockAfterExecution(
      self = self,
      block = block,
      stateRootHash = stateRootHash,
      receipts = receipts,
      gasUsed = gasUsed
    )
  }
}
