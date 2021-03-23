package io.iohk.ethereum.consensus.validators.std

import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator
import io.iohk.ethereum.consensus.validators.{BlockHeaderValidator, BlockValidator, SignedTransactionValidator}

/**
  * Implements validators that adhere to the PoW-specific
  * [[ValidatorsExecutor]]
  * interface.
  */
final class StdValidatorsExecutor private[std] (
    val blockValidator: BlockValidator,
    val blockHeaderValidator: BlockHeaderValidator,
    val signedTransactionValidator: SignedTransactionValidator,
    val ommersValidator: OmmersValidator
) extends ValidatorsExecutor
