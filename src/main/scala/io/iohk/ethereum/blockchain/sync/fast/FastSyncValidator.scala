package io.iohk.ethereum.blockchain.sync.fast

import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, Validators}
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}

class FastSyncValidator(
    val blockchain: Blockchain,
    val validators: Validators) extends SyncBlocksValidator with ReceiptsValidator {

  def validate(header: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    validators.blockHeaderValidator.validate(header, blockchain.getBlockHeaderByHash)
  }
}
