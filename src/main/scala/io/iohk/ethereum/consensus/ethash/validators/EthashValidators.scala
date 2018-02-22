package io.iohk.ethereum.consensus
package ethash.validators

import io.iohk.ethereum.consensus.validators.std.StdBlockValidator
import io.iohk.ethereum.consensus.validators.{BlockValidator, Validators}
import io.iohk.ethereum.utils.BlockchainConfig

trait EthashValidators extends Validators {
  def ommersValidator: OmmersValidator
}

class StdEthashValidators(
  blockchainConfig: BlockchainConfig,
  override val blockValidator: BlockValidator = StdBlockValidator
) extends validators.std.StdValidators(blockchainConfig, blockValidator) with EthashValidators {

  protected val _ommersV = new StdOmmersValidator(blockchainConfig, this._blockHeaderV)

  def ommersValidator: OmmersValidator = this._ommersV
}
