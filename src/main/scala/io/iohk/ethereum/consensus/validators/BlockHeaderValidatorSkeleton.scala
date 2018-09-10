package io.iohk.ethereum.consensus.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.validators.BlockHeaderError._
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig}

/**
 * A block header validator that does everything Ethereum prescribes except from:
 *  - PoW validation
 *  - Difficulty validation.
 *
 * The former is a characteristic of standard ethereum with Ethash, so it is not even known to
 * this implementation.
 *
 * The latter is treated polymorphically by directly using a difficulty
 * [[io.iohk.ethereum.consensus.difficulty.DifficultyCalculator calculator]].
 */
abstract class BlockHeaderValidatorSkeleton(blockchainConfig: BlockchainConfig) extends BlockHeaderValidator {

  import BlockHeaderValidator._

  /**
   * The difficulty calculator. This is specific to the consensus protocol.
   */
  protected def difficulty: DifficultyCalculator

  /**
   * A hook where even more consensus-specific validation can take place.
   * For example, PoW validation is done here.
   */
  protected def validateEvenMore(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid]

  /** This method allows validate a BlockHeader (stated on
   * section 4.4.4 of http://paper.gavwood.com/).
   *
   * @param blockHeader BlockHeader to validate.
   * @param parentHeader BlockHeader of the parent of the block to validate.
   */
  def validate(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    for {
      // NOTE how we include everything except PoW (which is deferred to `validateEvenMore`),
      //      and that difficulty validation is in effect abstract (due to `difficulty`).
      _ <- validateExtraData(blockHeader)
      _ <- validateTimestamp(blockHeader, parentHeader)
      _ <- validateDifficulty(blockHeader, parentHeader)
      _ <- validateGasUsed(blockHeader)
      _ <- validateGasLimit(blockHeader, parentHeader)
      _ <- validateNumber(blockHeader, parentHeader)
      _ <- validateEvenMore(blockHeader, parentHeader)
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
   * @return BlockHeader if valid, an [[io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderExtraDataError]] otherwise
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
      Left(HeaderGasLimitErrorBounds(
        minLimit = None,
        gasLimit = blockHeader.gasLimit,
        maxLimit = Some(MaxGasLimit: BigInt),
        eip106BlockNumber = Some(blockchainConfig.eip106BlockNumber),
        gasLimitDiff = None,
        gasLimitDiffLimit = None
      ))
    else blockchainConfig.constantBlockGasLimit match {
      case Some(constant) =>
        if (blockHeader.gasLimit == constant)
          Right(BlockHeaderValid)
        else
          Left(HeaderGasLimitErrorConst(constant = constant, gasLimit = blockHeader.gasLimit))

      case None =>
        val gasLimitDiff = (blockHeader.gasLimit - parentHeader.gasLimit).abs
        val gasLimitDiffLimit = parentHeader.gasLimit / GasLimitBoundDivisor
        if (gasLimitDiff < gasLimitDiffLimit && blockHeader.gasLimit >= MinGasLimit)
          Right(BlockHeaderValid)
        else
          Left(HeaderGasLimitErrorBounds(
            minLimit = Some(MinGasLimit),
            gasLimit = blockHeader.gasLimit,
            maxLimit = None,
            eip106BlockNumber = None,
            gasLimitDiff = Some(gasLimitDiff),
            gasLimitDiffLimit = Some(gasLimitDiffLimit)
          ))
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



