package io.iohk.ethereum.consensus.validators.std

import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.utils.BlockchainConfig

class StdValidators(
  _blockValidator: BlockValidator,
  _blockHeaderValidator: BlockHeaderValidator,
  _signedTransactionValidator: SignedTransactionValidator
) extends Validators {

  def blockValidator: BlockValidator = this._blockValidator

  def blockHeaderValidator: BlockHeaderValidator = this._blockHeaderValidator

  def signedTransactionValidator: SignedTransactionValidator = this._signedTransactionValidator
}

object StdValidators {
  def apply(blockchainConfig: BlockchainConfig): StdValidators =
    new StdValidators(
      StdBlockValidator,
      new StdBlockHeaderValidator(blockchainConfig),
      new StdSignedTransactionValidator(blockchainConfig)
    )
}
