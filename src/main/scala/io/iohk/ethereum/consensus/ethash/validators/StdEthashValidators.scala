package io.iohk.ethereum.consensus.ethash.validators

import io.iohk.ethereum.consensus.validators.std.{StdBlockValidator, StdSignedTransactionValidator}
import io.iohk.ethereum.consensus.validators.{BlockValidator, SignedTransactionValidator}
import io.iohk.ethereum.utils.BlockchainConfig

class StdEthashValidators(
  val blockValidator: BlockValidator,
  val blockHeaderValidator: EthashBlockHeaderValidator,
  val signedTransactionValidator: SignedTransactionValidator,
  val ommersValidator: OmmersValidator
) extends EthashValidators

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
