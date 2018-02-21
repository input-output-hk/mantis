package io.iohk.ethereum.consensus

import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.validators.{BlockHeaderValidator, OmmersValidator, SignedTransactionValidator}

trait Validators {
  def blockValidator: BlockValidator
  def blockHeaderValidator: BlockHeaderValidator
  // FIXME remove
  def ommersValidator: OmmersValidator
  def signedTransactionValidator: SignedTransactionValidator
}
