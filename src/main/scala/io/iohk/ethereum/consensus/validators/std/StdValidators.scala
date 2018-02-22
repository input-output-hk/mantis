package io.iohk.ethereum.consensus.validators.std

import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.utils.BlockchainConfig

class StdValidators(
  blockchainConfig: BlockchainConfig,
  val blockValidator: BlockValidator = StdBlockValidator
) extends Validators {

  private[this] val _blockHeaderV = new StdBlockHeaderValidator(blockchainConfig)
  private[this] val _ommersV = new StdOmmersValidator(blockchainConfig, this._blockHeaderV)
  private[this] val _signedTxV = new StdSignedTransactionValidator(blockchainConfig)

  def blockHeaderValidator: BlockHeaderValidator = this._blockHeaderV

  def ommersValidator: OmmersValidator = this._ommersV

  def signedTransactionValidator: SignedTransactionValidator = this._signedTxV
}
