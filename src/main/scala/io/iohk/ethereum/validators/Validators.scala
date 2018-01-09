package io.iohk.ethereum.validators

// FIXME Consider moving to [[io.iohk.ethereum.consensus.Consensus]]
trait Validators {

  val blockValidator: BlockValidator
  val blockHeaderValidator: BlockHeaderValidator
  val ommersValidator: OmmersValidator
  val signedTransactionValidator: SignedTransactionValidator

}
 