package io.iohk.ethereum.consensus.atomixraft.validators

import io.iohk.ethereum.consensus.validators.std.{StdBlockValidator, StdSignedTransactionValidator, StdValidators}
import io.iohk.ethereum.utils.BlockchainConfig


object AtomixRaftValidators {
  def apply(blockchainConfig: BlockchainConfig): AtomixRaftValidators = {
    new StdValidators(
      new StdBlockValidator(blockchainConfig.ethCompatibilityMode),
      new AtomixRaftBlockHeaderValidator(blockchainConfig),
      new StdSignedTransactionValidator(blockchainConfig)
    )
  }
}
