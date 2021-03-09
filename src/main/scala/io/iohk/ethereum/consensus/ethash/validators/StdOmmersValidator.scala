package io.iohk.ethereum.consensus.ethash.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.GetBlockHeaderByHash
import io.iohk.ethereum.consensus.GetNBlocksBack
import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator.OmmersError
import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator.OmmersError._
import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator.OmmersValid
import io.iohk.ethereum.consensus.validators.BlockHeaderValidator
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class StdOmmersValidator(blockchainConfig: BlockchainConfig, blockHeaderValidator: BlockHeaderValidator)
    extends OmmersValidator {

  val OmmerGenerationLimit: Int = 6 // Stated on section 11.1, eq. (143) of the YP
  val OmmerSizeLimit: Int = 2

  /** This method allows validating the ommers of a Block. It performs the following validations (stated on
    * section 11.1 of the YP):
    *   - OmmersValidator.validateOmmersLength
    *   - OmmersValidator.validateOmmersHeaders
    *   - OmmersValidator.validateOmmersAncestors
    * It also includes validations mentioned in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    * and implemented in the different ETC clients:
    *   - OmmersValidator.validateOmmersNotUsed
    *   - OmmersValidator.validateDuplicatedOmmers
    *
    * @param parentHash           the hash of the parent of the block to which the ommers belong
    * @param blockNumber          the number of the block to which the ommers belong
    * @param ommers               the list of ommers to validate
    * @param getBlockHeaderByHash function to obtain an ancestor block header by hash
    * @param getNBlocksBack       function to obtain N blocks including one given by hash and its N-1 ancestors
    *
    * @return [[OmmersValidator.OmmersValid]] if valid, an [[OmmersValidator.OmmersError]] otherwise
    */
  def validate(
      parentHash: ByteString,
      blockNumber: BigInt,
      ommers: Seq[BlockHeader],
      getBlockHeaderByHash: GetBlockHeaderByHash,
      getNBlocksBack: GetNBlocksBack
  ): Either[OmmersError, OmmersValid] =
    if (ommers.isEmpty)
      Right(OmmersValid)
    else
      for {
        _ <- validateOmmersLength(ommers)
        _ <- validateDuplicatedOmmers(ommers)
        _ <- validateOmmersHeaders(ommers, getBlockHeaderByHash)
        _ <- validateOmmersAncestors(parentHash, blockNumber, ommers, getNBlocksBack)
        _ <- validateOmmersNotUsed(parentHash, blockNumber, ommers, getNBlocksBack)
      } yield OmmersValid

  /** Validates ommers length based on validations stated in section 11.1 of the YP
    *
    * @param ommers the list of ommers to validate
    *
    * @return [[OmmersValidator.OmmersValid]] if valid, an [[OmmersValidator.OmmersError.OmmersLengthError]] otherwise
    */
  private def validateOmmersLength(ommers: Seq[BlockHeader]): Either[OmmersError, OmmersValid] =
    if (ommers.length <= OmmerSizeLimit) Right(OmmersValid)
    else Left(OmmersLengthError)

  /** Validates that each ommer's header is valid based on validations stated in section 11.1 of the YP
    *
    * @param ommers               the list of ommers to validate
    * @param getBlockHeaderByHash function to obtain ommers' parents
    *
    * @return [[OmmersValidator.OmmersValid]] if valid, an [[OmmersValidator.OmmersError.OmmersNotValidError]] otherwise
    */
  private def validateOmmersHeaders(
      ommers: Seq[BlockHeader],
      getBlockHeaderByHash: GetBlockHeaderByHash
  ): Either[OmmersError, OmmersValid] =
    if (ommers.forall(blockHeaderValidator.validate(_, getBlockHeaderByHash).isRight)) Right(OmmersValid)
    else Left(OmmersNotValidError)

  /** Validates that each ommer is not too old and that it is a sibling as one of the current block's ancestors
    * based on validations stated in section 11.1 of the YP
    *
    * @param parentHash     the hash of the parent of the block to which the ommers belong
    * @param blockNumber    the number of the block to which the ommers belong
    * @param ommers         the list of ommers to validate
    * @param getNBlocksBack from where the ommers' parents will be obtained
    *
    * @return [[OmmersValidator.OmmersValid]] if valid, an [[OmmersValidator.OmmersError.OmmersUsedBeforeError]] otherwise
    */
  private def validateOmmersAncestors(
      parentHash: ByteString,
      blockNumber: BigInt,
      ommers: Seq[BlockHeader],
      getNBlocksBack: GetNBlocksBack
  ): Either[OmmersError, OmmersValid] = {

    val ancestorsOpt = collectAncestors(parentHash, blockNumber, getNBlocksBack)

    val validOmmersAncestors: Seq[BlockHeader] => Boolean = ancestors =>
      ommers.forall { ommer =>
        val ommerIsNotAncestor = ancestors.forall(_.hash != ommer.hash)
        val ommersParentIsAncestor = ancestors.exists(_.parentHash == ommer.parentHash)
        ommerIsNotAncestor && ommersParentIsAncestor
      }
    if (ancestorsOpt.exists(validOmmersAncestors)) Right(OmmersValid)
    else Left(OmmersAncestorsError)
  }

  /** Validates that each ommer was not previously used
    * based on validations stated in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    *
    * @param parentHash     the hash of the parent of the block to which the ommers belong
    * @param blockNumber    the number of the block to which the ommers belong
    * @param ommers         the list of ommers to validate
    * @param getNBlocksBack from where the ommers' parents will be obtained
    * @return [[OmmersValidator.OmmersValid]] if valid, an [[OmmersValidator.OmmersError.OmmersUsedBeforeError]] otherwise
    */
  private def validateOmmersNotUsed(
      parentHash: ByteString,
      blockNumber: BigInt,
      ommers: Seq[BlockHeader],
      getNBlocksBack: GetNBlocksBack
  ): Either[OmmersError, OmmersValid] = {

    val ommersFromAncestorsOpt = collectOmmersFromAncestors(parentHash, blockNumber, getNBlocksBack)

    if (ommersFromAncestorsOpt.exists(ommers.intersect(_).isEmpty)) Right(OmmersValid)
    else Left(OmmersUsedBeforeError)
  }

  /** Validates that there are no duplicated ommers
    * based on validations stated in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    *
    * @param ommers the list of ommers to validate
    * @return [[OmmersValidator.OmmersValid]] if valid, an [[OmmersValidator.OmmersError.OmmersDuplicatedError]] otherwise
    */
  private def validateDuplicatedOmmers(ommers: Seq[BlockHeader]): Either[OmmersError, OmmersValid] =
    if (ommers.distinct.length == ommers.length) Right(OmmersValid)
    else Left(OmmersDuplicatedError)

  private def collectAncestors(
      parentHash: ByteString,
      blockNumber: BigInt,
      getNBlocksBack: GetNBlocksBack
  ): Option[Seq[BlockHeader]] = {
    val numberOfBlocks = blockNumber.min(OmmerGenerationLimit).toInt
    val ancestors = getNBlocksBack(parentHash, numberOfBlocks).map(_.header)
    Some(ancestors).filter(_.length == numberOfBlocks)
  }

  private def collectOmmersFromAncestors(
      parentHash: ByteString,
      blockNumber: BigInt,
      getNBlocksBack: GetNBlocksBack
  ): Option[Seq[BlockHeader]] = {
    val numberOfBlocks = blockNumber.min(OmmerGenerationLimit).toInt
    val ancestors = getNBlocksBack(parentHash, numberOfBlocks).map(_.body.uncleNodesList)
    Some(ancestors).filter(_.length == numberOfBlocks).map(_.flatten)
  }
}
