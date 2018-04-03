package io.iohk.ethereum.consensus.ethash.validators

import io.iohk.ethereum.consensus.validators.{BlockValidator, SignedTransactionValidator}

/**
 * Implements validators that adhere to the PoW-specific
 * [[io.iohk.ethereum.consensus.ethash.validators.EthashValidators EthashValidators]]
 * interface.
 */
final class StdEthashValidators private[validators](
  val blockValidator: BlockValidator,
  val blockHeaderValidator: EthashBlockHeaderValidator,
  val signedTransactionValidator: SignedTransactionValidator,
  val ommersValidator: OmmersValidator
) extends EthashValidators
