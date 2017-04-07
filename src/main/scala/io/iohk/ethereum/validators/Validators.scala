package io.iohk.ethereum.validators

trait Validators {

  val blockValidator: BlockValidator
  val blockHeaderValidator: BlockHeaderValidator
  val ommersValidator: OmmersValidator
  val signedTransactionValidator: SignedTransactionValidator

}

object ValidatorsImpl extends Validators {

  override val blockValidator: BlockValidator = BlockValidator
  override val blockHeaderValidator: BlockHeaderValidator = BlockHeaderValidator
  override val ommersValidator: OmmersValidator = OmmersValidator
  override val signedTransactionValidator: SignedTransactionValidator = SignedTransactionValidator

}
