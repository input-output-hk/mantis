package io.iohk.ethereum.consensus.pow.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator.OmmersError._
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator.{OmmersError, OmmersValid}
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidator}
import io.iohk.ethereum.consensus.{GetBlockHeaderByHash, GetNBlocksBack}
import io.iohk.ethereum.domain.BlockHeader

class StdOmmersValidator(blockHeaderValidator: BlockHeaderValidator) extends OmmersValidator {

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
    * @return [[io.iohk.ethereum.consensus.pow.validators.OmmersValidator.OmmersValid]] if valid,
    *         an [[io.iohk.ethereum.consensus.pow.validators.OmmersValidator.OmmersError]] otherwise
    */
  def validate(
      parentHash: ByteString,
      blockNumber: BigInt,
      ommers: Seq[BlockHeader],
      getBlockHeaderByHash: GetBlockHeaderByHash,
      getNBlocksBack: GetNBlocksBack
  ): Either[OmmersError, OmmersValid] = {

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
  }

  /** Validates ommers length based on validations stated in section 11.1 of the YP
    *
    * @param ommers the list of ommers to validate
    *
    * @return [[OmmersValidator.OmmersValid]] if valid, an [[OmmersValidator.OmmersError.OmmersLengthError]] otherwise
    */
  private def validateOmmersLength(ommers: Seq[BlockHeader]): Either[OmmersError, OmmersValid] = {
    if (ommers.length <= OmmerSizeLimit) Right(OmmersValid)
    else Left(OmmersLengthError)
  }

  /** Validates that each ommer's header is valid based on validations stated in section 11.1 of the YP
    *
    * @param ommers               the list of ommers to validate
    * @param getBlockParentsHeaderByHash function to obtain ommers' parents
    * @return [[OmmersValidator.OmmersValid]] if valid, an [[OmmersValidator.OmmersError.OmmersHeaderError]] otherwise
    */
  private def validateOmmersHeaders(
      ommers: Seq[BlockHeader],
      getBlockParentsHeaderByHash: GetBlockHeaderByHash
  ): Either[OmmersError, OmmersValid] = {
    val validationsResult: Seq[Either[BlockHeaderError, BlockHeaderValid]] =
      ommers.map(blockHeaderValidator.validate(_, getBlockParentsHeaderByHash))

    if (validationsResult.forall(_.isRight)) Right(OmmersValid)
    else {
      val errors = validationsResult.collect { case Left(error) => error }.toList
      Left(OmmersHeaderError(errors))
    }
  }

  /** Validates that each ommer is not too old and that it is a sibling as one of the current block's ancestors
    * based on validations stated in section 11.1 of the YP
    *
    * @param parentHash     the hash of the parent of the block to which the ommers belong
    * @param blockNumber    the number of the block to which the ommers belong
    * @param ommers         the list of ommers to validate
    * @param getNBlocksBack from where the ommers' parents will be obtained
    * @return [[OmmersValidator.OmmersValid]] if valid, an [[OmmersValidator.OmmersError.OmmerIsAncestorError]] or
    *         [[OmmersValidator.OmmersError.OmmerParentIsNotAncestorError]]  otherwise
    */
  private[validators] def validateOmmersAncestors(
      parentHash: ByteString,
      blockNumber: BigInt,
      ommers: Seq[BlockHeader],
      getNBlocksBack: GetNBlocksBack
  ): Either[OmmersError, OmmersValid] = {

    val ancestors = collectAncestors(parentHash, blockNumber, getNBlocksBack)
    lazy val ommersHashes: Seq[ByteString] = ommers.map(_.hash)
    lazy val ommersThatAreAncestors: Seq[ByteString] = ancestors.map(_.hash).intersect(ommersHashes)

    lazy val ancestorsParents: Seq[ByteString] = ancestors.map(_.parentHash)
    lazy val ommersParentsHashes: Seq[ByteString] = ommers.map(_.parentHash)

    // parent not an ancestor or is too old (we only compare up to 6 previous ancestors)
    lazy val ommersParentsAreAllAncestors: Boolean = ommersParentsHashes.forall(ancestorsParents.contains)

    if (ommersThatAreAncestors.nonEmpty) Left(OmmerIsAncestorError)
    else if (!ommersParentsAreAllAncestors) Left(OmmerParentIsNotAncestorError)
    else Right(OmmersValid)
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

    val ommersFromAncestors = collectOmmersFromAncestors(parentHash, blockNumber, getNBlocksBack)

    if (ommers.intersect(ommersFromAncestors).isEmpty) Right(OmmersValid)
    else Left(OmmersUsedBeforeError)
  }

  /** Validates that there are no duplicated ommers
    * based on validations stated in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    *
    * @param ommers the list of ommers to validate
    * @return [[OmmersValidator.OmmersValid]] if valid, an [[OmmersValidator.OmmersError.OmmersDuplicatedError]] otherwise
    */
  private def validateDuplicatedOmmers(ommers: Seq[BlockHeader]): Either[OmmersError, OmmersValid] = {
    if (ommers.distinct.length == ommers.length) Right(OmmersValid)
    else Left(OmmersDuplicatedError)
  }

  private def collectAncestors(
      parentHash: ByteString,
      blockNumber: BigInt,
      getNBlocksBack: GetNBlocksBack
  ): Seq[BlockHeader] = {
    val numberOfBlocks = blockNumber.min(OmmerGenerationLimit).toInt
    getNBlocksBack(parentHash, numberOfBlocks).map(_.header)
  }

  private def collectOmmersFromAncestors(
      parentHash: ByteString,
      blockNumber: BigInt,
      getNBlocksBack: GetNBlocksBack
  ): Seq[BlockHeader] = {
    val numberOfBlocks = blockNumber.min(OmmerGenerationLimit).toInt
    getNBlocksBack(parentHash, numberOfBlocks).flatMap(_.body.uncleNodesList)
  }
}
