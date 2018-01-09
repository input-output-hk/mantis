package io.iohk.ethereum.consensus.validators
package std

import akka.util.ByteString
import io.iohk.ethereum.domain.{BlockHeader, DifficultyCalculator}
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig}

// FIXME Decouple PoW validation
class StdBlockHeaderValidator(blockchainConfig: BlockchainConfig) extends BlockHeaderValidator {

  import BlockHeaderError._
  import StdBlockHeaderValidator._

  // FIXME Reflect the need for being concurrent
  // we need concurrent map since validators can be used from multiple places
  val powCaches: java.util.concurrent.ConcurrentMap[Long, PowCacheData] = new java.util.concurrent.ConcurrentHashMap[Long, PowCacheData]()

  val difficulty = new DifficultyCalculator(blockchainConfig)

  /** This method allows validate a BlockHeader (stated on
   * section 4.4.4 of http://paper.gavwood.com/).
   *
   * @param blockHeader BlockHeader to validate.
   * @param parentHeader BlockHeader of the parent of the block to validate.
   */
  // FIXME Promote to the interface? (but there will be compilation errors ...)
  def validate(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    for {
      _ <- validateExtraData(blockHeader)
      _ <- validateTimestamp(blockHeader, parentHeader)
      _ <- validateDifficulty(blockHeader, parentHeader)
      _ <- validateGasUsed(blockHeader)
      _ <- validateGasLimit(blockHeader, parentHeader)
      _ <- validateNumber(blockHeader, parentHeader)
    } yield BlockHeaderValid
  }

  /** This method allows validate a BlockHeader (stated on
   * section 4.4.4 of http://paper.gavwood.com/).
   *
   * @param blockHeader BlockHeader to validate.
   * @param getBlockHeaderByHash function to obtain the parent header.
   */
  def validate(blockHeader: BlockHeader, getBlockHeaderByHash: ByteString => Option[BlockHeader]): Either[BlockHeaderError, BlockHeaderValid] = {
    for {
      blockHeaderParent <- getBlockHeaderByHash(blockHeader.parentHash).map(Right(_)).getOrElse(Left(HeaderParentNotFoundError))
      _ <- validate(blockHeader, blockHeaderParent)
    } yield BlockHeaderValid
  }

  /**
   * Validates [[io.iohk.ethereum.domain.BlockHeader.extraData]] length
   * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
   *
   * @param blockHeader BlockHeader to validate.
   * @return BlockHeader if valid, an [[HeaderExtraDataError]] otherwise
   */
  private def validateExtraData(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {

    def validateDaoForkExtraData(blockHeader: BlockHeader, daoForkConfig: DaoForkConfig): Either[BlockHeaderError, BlockHeaderValid] =
      (daoForkConfig requiresExtraData blockHeader.number, daoForkConfig.blockExtraData) match {
        case (false, _) =>
          Right(BlockHeaderValid)
        case (true, Some(forkExtraData)) if blockHeader.extraData == forkExtraData =>
          Right(BlockHeaderValid)
        case _ =>
          Left(DaoHeaderExtraDataError)
      }

    if (blockHeader.extraData.length <= MaxExtraDataSize) {
      import blockchainConfig._
      daoForkConfig.map(c => validateDaoForkExtraData(blockHeader, c)).getOrElse(Right(BlockHeaderValid))
    } else {
      Left(HeaderExtraDataError)
    }
  }

  /**
   * Validates [[io.iohk.ethereum.domain.BlockHeader.unixTimestamp]] is greater than the one of its parent
   * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
   *
   * @param blockHeader BlockHeader to validate.
   * @param parentHeader BlockHeader of the parent of the block to validate.
   * @return BlockHeader if valid, an [[HeaderTimestampError]] otherwise
   */
  private def validateTimestamp(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    if(blockHeader.unixTimestamp > parentHeader.unixTimestamp) Right(BlockHeaderValid)
    else Left(HeaderTimestampError)

  /**
   * Validates [[io.iohk.ethereum.domain.BlockHeader.difficulty]] is correct
   * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
   *
   * @param blockHeader BlockHeader to validate.
   * @param parentHeader BlockHeader of the parent of the block to validate.
   * @return BlockHeader if valid, an [[HeaderDifficultyError]] otherwise
   */
  private def validateDifficulty(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    if (difficulty.calculateDifficulty(blockHeader.number, blockHeader.unixTimestamp, parentHeader) == blockHeader.difficulty) Right(BlockHeaderValid)
    else Left(HeaderDifficultyError)

  /**
   * Validates [[io.iohk.ethereum.domain.BlockHeader.gasUsed]] is not greater than [[io.iohk.ethereum.domain.BlockHeader.gasLimit]]
   * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
   *
   * @param blockHeader BlockHeader to validate.
   * @return BlockHeader if valid, an [[HeaderGasUsedError]] otherwise
   */
  private def validateGasUsed(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    if(blockHeader.gasUsed<=blockHeader.gasLimit && blockHeader.gasUsed >= 0) Right(BlockHeaderValid)
    else Left(HeaderGasUsedError)

  /**
   * Validates [[io.iohk.ethereum.domain.BlockHeader.gasLimit]] follows the restrictions based on its parent gasLimit
   * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
   *
   * EIP106(https://github.com/ethereum/EIPs/issues/106) adds additional validation of maximum value for gasLimit.
   *
   * @param blockHeader BlockHeader to validate.
   * @param parentHeader BlockHeader of the parent of the block to validate.
   * @return BlockHeader if valid, an [[HeaderGasLimitError]] otherwise
   */
  private def validateGasLimit(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {

    if (blockHeader.gasLimit > MaxGasLimit && blockHeader.number >= blockchainConfig.eip106BlockNumber)
      Left(HeaderGasLimitError)
    else {
      val gasLimitDiff = (blockHeader.gasLimit - parentHeader.gasLimit).abs
      val gasLimitDiffLimit = parentHeader.gasLimit / GasLimitBoundDivisor
      if (gasLimitDiff < gasLimitDiffLimit && blockHeader.gasLimit >= MinGasLimit)
        Right(BlockHeaderValid)
      else
        Left(HeaderGasLimitError)
    }
  }

  /**
   * Validates [[io.iohk.ethereum.domain.BlockHeader.number]] is the next one after its parents number
   * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
   *
   * @param blockHeader BlockHeader to validate.
   * @param parentHeader BlockHeader of the parent of the block to validate.
   * @return BlockHeader if valid, an [[HeaderNumberError]] otherwise
   */
  private def validateNumber(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    if(blockHeader.number == parentHeader.number + 1) Right(BlockHeaderValid)
    else Left(HeaderNumberError)
}

object StdBlockHeaderValidator {
  val MaxExtraDataSize: Int = 32
  val GasLimitBoundDivisor: Int = 1024
  val MinGasLimit: BigInt = 5000 //Although the paper states this value is 125000, on the different clients 5000 is used
  val MaxGasLimit = Long.MaxValue // max gasLimit is equal 2^63-1 according to EIP106

  class PowCacheData(val cache: Array[Int], val dagSize: Long)

}
