package io.iohk.ethereum.consensus

import io.iohk.ethereum.validators.{BlockHeaderValidator, BlockValidator, OmmersValidator, SignedTransactionValidator}

trait Validators {
  def blockValidator: BlockValidator
  def blockHeaderValidator: BlockHeaderValidator
  // FIXME remove
  def ommersValidator: OmmersValidator
  def signedTransactionValidator: SignedTransactionValidator
}
