package io.iohk.ethereum.consensus.pow.validators

import io.iohk.ethereum.consensus.pow.KeccakCalculation
import io.iohk.ethereum.consensus.validators.BlockHeaderError
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderPoWError
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain.BlockHeader

object KeccakBlockHeaderValidator {

  /** Validates [[io.iohk.ethereum.domain.BlockHeader.nonce]] and [[io.iohk.ethereum.domain.BlockHeader.mixHash]] are correct
    * @param blockHeader
    * @return BlockHeaderValid if valid or an BlockHeaderError.HeaderPoWError otherwise
    */
  def validateHeader(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    val rlpEncodedHeader = BlockHeader.getEncodedWithoutNonce(blockHeader)
    val expectedHash = KeccakCalculation.hash(rlpEncodedHeader, BigInt(blockHeader.nonce.toArray))

    lazy val isDifficultyValid = KeccakCalculation.isMixHashValid(blockHeader.mixHash, blockHeader.difficulty)

    if (expectedHash.mixHash == blockHeader.mixHash && isDifficultyValid) Right(BlockHeaderValid)
    else Left(HeaderPoWError)
  }
}
