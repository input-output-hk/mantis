package io.iohk.ethereum.consensus.validators.std

import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.utils.BlockchainConfig

class StdValidators(
  blockchainConfig: BlockchainConfig,
  val blockValidator: BlockValidator = StdBlockValidator
) extends Validators {

  protected val _blockHeaderV = new StdBlockHeaderValidator(blockchainConfig)
  protected val _signedTxV = new StdSignedTransactionValidator(blockchainConfig)

  def blockHeaderValidator: BlockHeaderValidator = this._blockHeaderV

  def signedTransactionValidator: SignedTransactionValidator = this._signedTxV
}
