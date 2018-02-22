package io.iohk.ethereum.consensus.validators

trait Validators {
  def blockValidator: BlockValidator
  def blockHeaderValidator: BlockHeaderValidator
  // FIXME remove
  def ommersValidator: OmmersValidator
  def signedTransactionValidator: SignedTransactionValidator
}
