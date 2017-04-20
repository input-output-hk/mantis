package io.iohk.ethereum.validators

trait Validators {

  val blockValidator: BlockValidator
  val blockHeaderValidator: BlockHeaderValidator
  val ommersValidator: OmmersValidator
  val signedTransactionValidator: SignedTransactionValidator

}
