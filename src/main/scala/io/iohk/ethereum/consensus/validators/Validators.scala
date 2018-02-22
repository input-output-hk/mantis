package io.iohk.ethereum.consensus.validators

trait Validators {
  def blockValidator: BlockValidator
  def blockHeaderValidator: BlockHeaderValidator
  def signedTransactionValidator: SignedTransactionValidator
}
