package io.iohk.ethereum.consensus.pow.validators

import io.iohk.ethereum.consensus.validators.{BlockHeaderValidator, BlockValidator, SignedTransactionValidator}

/**
  * Implements validators that adhere to the PoW-specific
  * [[io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor]]
  * interface.
  */
final class StdValidatorsExecutor private[validators] (
    val blockValidator: BlockValidator,
    val blockHeaderValidator: BlockHeaderValidator,
    val signedTransactionValidator: SignedTransactionValidator,
    val ommersValidator: OmmersValidator
) extends ValidatorsExecutor
