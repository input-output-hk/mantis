package io.iohk.ethereum.consensus

import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.utils.BlockchainConfig

trait Validators {
  def blockValidator: BlockValidator
  def blockHeaderValidator: BlockHeaderValidator
  // FIXME remove
  def ommersValidator: OmmersValidator
  def signedTransactionValidator: SignedTransactionValidator
}

class StdValidators(
  blockchainConfig: BlockchainConfig,
  val blockValidator: BlockValidator = BlockValidator
) extends Validators {

  private[this] val _blockHeaderV = new BlockHeaderValidatorImpl(blockchainConfig)
  private[this] val _ommersV = new OmmersValidatorImpl(blockchainConfig, this._blockHeaderV)
  private[this] val _signedTxV = new SignedTransactionValidatorImpl(blockchainConfig)

  def blockHeaderValidator: BlockHeaderValidator = this._blockHeaderV

  def ommersValidator: OmmersValidator = this._ommersV

  def signedTransactionValidator: SignedTransactionValidator = this._signedTxV
}
