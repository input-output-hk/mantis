package io.iohk.ethereum.consensus.ethash.validators

import io.iohk.ethereum.consensus.validators
import io.iohk.ethereum.consensus.validators.std.{StdBlockValidator, StdSignedTransactionValidator}
import io.iohk.ethereum.consensus.validators.{BlockValidator, SignedTransactionValidator}
import io.iohk.ethereum.utils.BlockchainConfig

class StdEthashValidators(
  _blockValidator: BlockValidator,
  _blockHeaderValidator: EthashBlockHeaderValidator,
  _signedTransactionValidator: SignedTransactionValidator,
  _ommersValidator: OmmersValidator
) extends validators.std.StdValidators(
  _blockValidator, _blockHeaderValidator, _signedTransactionValidator
) with EthashValidators {

  def ommersValidator: OmmersValidator = this._ommersValidator

  override def blockHeaderValidator: EthashBlockHeaderValidator = this._blockHeaderValidator
}

object StdEthashValidators {
  def apply(blockchainConfig: BlockchainConfig): StdEthashValidators = {
    val blockHeaderValidator = new EthashBlockHeaderValidator(blockchainConfig)

    new StdEthashValidators(
      StdBlockValidator,
      blockHeaderValidator,
      new StdSignedTransactionValidator(blockchainConfig),
      new StdOmmersValidator(blockchainConfig, blockHeaderValidator)
    )
  }
}
