package io.iohk.ethereum.validators

import akka.util.ByteString
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.validators.OmmersValidator.OmmersError._
import io.iohk.ethereum.validators.OmmersValidator.{GetBlockHeaderByHash, GetNBlocksBack, OmmersError, OmmersValid}

trait OmmersValidator {

  def validate(
    parentHash: ByteString,
    blockNumber: BigInt,
    ommers: Seq[BlockHeader],
    getBlockHeaderByHash: GetBlockHeaderByHash,
    getNBlocksBack: GetNBlocksBack): Either[OmmersError, OmmersValid]

  def validate(
    parentHash: ByteString,
    blockNumber: BigInt,
    ommers: Seq[BlockHeader],
    blockchain: Blockchain): Either[OmmersError, OmmersValid] = {

    val getBlockHeaderByHash: ByteString => Option[BlockHeader] = blockchain.getBlockHeaderByHash
    val getNBlocksBack: (ByteString, Int) => List[Block] =
      (_, n) => ((blockNumber - n) until blockNumber).toList.flatMap(blockchain.getBlockByNumber)

    validate(parentHash, blockNumber, ommers, getBlockHeaderByHash, getNBlocksBack)
  }

}

object OmmersValidator {
  sealed trait OmmersError

  object OmmersError {
    case object OmmersLengthError extends OmmersError
    case object OmmersNotValidError extends OmmersError
    case object OmmersUsedBeforeError extends OmmersError
    case object OmmersAncestorsError extends OmmersError
    case object OmmersDuplicatedError extends OmmersError
  }

  sealed trait OmmersValid
  case object OmmersValid extends OmmersValid

  type GetBlockHeaderByHash = ByteString => Option[BlockHeader]
  type GetNBlocksBack = (ByteString, Int) => Seq[Block]
}

class OmmersValidatorImpl(blockchainConfig: BlockchainConfig, blockHeaderValidator: BlockHeaderValidator) extends OmmersValidator {

  val OmmerGenerationLimit: Int = 6 //Stated on section 11.1, eq. (143) of the YP
  val OmmerSizeLimit: Int = 2

  // FIXME: scaladoc
  /**
    * This method allows validating the ommers of a Block. It performs the following validations (stated on
    * section 11.1 of the YP):
    *   - OmmersValidator.validateOmmersLength
    *   - OmmersValidator.validateOmmersHeaders
    *   - OmmersValidator.validateOmmersAncestors
    * It also includes validations mentioned in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    * and implemented in the different ETC clients:
    *   - OmmersValidator.validateOmmersNotUsed
    *   - OmmersValidator.validateDuplicatedOmmers
    *
    * @param parentHash    The hash of the parent of the block to which the ommers belong
    * @param blockNumber    The number of the block to which the ommers belong
    * @param ommers         The list of ommers to validate
    * @param getBlockHeaderByHash function to obtain an ancestor block header by hash
    * @param getNBlocksBack function to obtain N blocks including one given by hash and its N-1 ancestors
    * @return ommers if valid, an [[OmmersValidator.OmmersError]] otherwise
    */
  def validate(
    parentHash: ByteString,
    blockNumber: BigInt,
    ommers: Seq[BlockHeader],
    getBlockHeaderByHash: GetBlockHeaderByHash,
    getNBlocksBack: GetNBlocksBack): Either[OmmersError, OmmersValid] = {

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

  // FIXME: scaladoc
  /**
    * Validates ommers length
    * based on validations stated in section 11.1 of the YP
    *
    * @param ommers         The list of ommers to validate
    * @return ommers if valid, an [[OmmersLengthError]] otherwise
    */
  private def validateOmmersLength(ommers: Seq[BlockHeader]): Either[OmmersError, OmmersValid] = {
    if (ommers.length <= OmmerSizeLimit) Right(OmmersValid)
    else Left(OmmersLengthError)
  }

  // FIXME: scaladoc
  /**
    * Validates that each ommer's header is valid
    * based on validations stated in section 11.1 of the YP
    *
    * @param ommers         The list of ommers to validate
    * @param getBlockByHash     function to obtain ommers' parents
    * @return ommers if valid, an [[OmmersNotValidError]] otherwise
    */
  private def validateOmmersHeaders(ommers: Seq[BlockHeader], getBlockByHash: GetBlockHeaderByHash): Either[OmmersError, OmmersValid] = {
    if (ommers.forall(blockHeaderValidator.validate(_, getBlockByHash).isRight)) Right(OmmersValid)
    else Left(OmmersNotValidError)
  }

  // FIXME: scaladoc
  /**
    * Validates that each ommer is not too old and that it is a sibling as one of the current block's ancestors
    * based on validations stated in section 11.1 of the YP
    *
    * @param parentHash  The hash of the parent of the block to which the ommers belong
    * @param blockNumber    The number of the block to which the ommers belong
    * @param ommers         The list of ommers to validate
    * @param getNBlocksBack     from where the ommers' parents will be obtained
    * @return ommers if valid, an [[OmmersUsedBeforeError]] otherwise
    */
  private def validateOmmersAncestors(
    parentHash: ByteString,
    blockNumber: BigInt,
    ommers: Seq[BlockHeader],
    getNBlocksBack: GetNBlocksBack): Either[OmmersError, OmmersValid] = {

    val ancestorsOpt = collectAncestors(parentHash, blockNumber, getNBlocksBack)

    val validOmmersAncestors: Seq[BlockHeader] => Boolean = ancestors =>
      ommers.forall{ ommer =>
        val ommerIsNotAncestor = ancestors.forall(_.hash != ommer.hash)
        val ommersParentIsAncestor = ancestors.exists(_.parentHash == ommer.parentHash)
        ommerIsNotAncestor && ommersParentIsAncestor
      }
    if (ancestorsOpt.exists(validOmmersAncestors)) Right(OmmersValid)
    else Left(OmmersAncestorsError)
  }

  // FIXME: scaladoc
  /**
    * Validates that each ommer was not previously used
    * based on validations stated in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    *
    * @param parentHash  The hash of the parent of the block to which the ommers belong
    * @param blockNumber    The number of the block to which the ommers belong
    * @param ommers         The list of ommers to validate
    * @param getNBlocksBack     from where the ommers' parents will be obtained
    * @return ommers if valid, an [[OmmersUsedBeforeError]] otherwise
    */
  private def validateOmmersNotUsed(
    parentHash: ByteString,
    blockNumber: BigInt,
    ommers: Seq[BlockHeader],
    getNBlocksBack: GetNBlocksBack): Either[OmmersError, OmmersValid] = {

    val ommersFromAncestorsOpt = collectOmmersFromAncestors(parentHash, blockNumber, getNBlocksBack)

    if (ommersFromAncestorsOpt.exists(ommers.intersect(_).isEmpty)) Right(OmmersValid)
    else Left(OmmersUsedBeforeError)
  }

  // FIXME: scaladoc
  /**
    * Validates that there are no duplicated ommers
    * based on validations stated in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    *
    * @param ommers         The list of ommers to validate
    * @return ommers if valid, an [[OmmersDuplicatedError]] otherwise
    */
  private def validateDuplicatedOmmers(ommers: Seq[BlockHeader]): Either[OmmersError, OmmersValid] = {
    if (ommers.distinct.length == ommers.length) Right(OmmersValid)
    else Left(OmmersDuplicatedError)
  }

  private def collectAncestors(parentHash: ByteString, blockNumber: BigInt, getNBlocksBack: GetNBlocksBack): Option[Seq[BlockHeader]] = {
    val numberOfBlocks = blockNumber.min(OmmerGenerationLimit).toInt
    val ancestors = getNBlocksBack(parentHash, numberOfBlocks).map(_.header)
    Some(ancestors).filter(_.length == numberOfBlocks)
  }

  private def collectOmmersFromAncestors(parentHash: ByteString, blockNumber: BigInt, getNBlocksBack: GetNBlocksBack): Option[Seq[BlockHeader]] = {
    val numberOfBlocks = blockNumber.min(OmmerGenerationLimit).toInt
    val ancestors = getNBlocksBack(parentHash, numberOfBlocks).map(_.body.uncleNodesList)
    Some(ancestors).filter(_.length == numberOfBlocks).map(_.flatten)
  }
}
