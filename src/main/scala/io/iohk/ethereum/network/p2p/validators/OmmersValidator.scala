package io.iohk.ethereum.network.p2p.validators

import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.network.p2p.validators.OmmersValidator.OmmersError._

object OmmersValidator {

  val OmmerGenerationLimit: Int = 6
  val OmmerSizeLimit: Int = 2

  /**
    * This method allows validating the ommers of a Block. It performs the following validations (stated on
    * section 11.1 of http://paper.gavwood.com/):
    *   - OmmersValidator.validateOmmersLength
    *   - OmmersValidator.validateOmmersHeaders
    *   - OmmersValidator.validateOmmersAncestors
    * It also includes validations mentioned in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    * and implemented in the different ETC clients:
    *   - OmmersValidator.validateOmmersNotUsed
    *   - OmmersValidator.validateDuplicatedOmmers
    * @param blockNumber    The number of the block to which the ommers belong
    * @param ommers         The list of ommers to validate
    * @param blockchain     from where the previous blocks are obtained
    * @return ommers if valid, an [[OmmersError]] otherwise
    */
  def validate(blockNumber: BigInt, ommers: Seq[BlockHeader], blockchain: Blockchain): Either[OmmersError, Seq[BlockHeader]] = {
    for {
      _ <- validateOmmersLength(ommers)
      _ <- validateOmmersHeaders(ommers, blockchain)
      _ <- validateOmmersAncestors(blockNumber, ommers, blockchain)
      _ <- validateOmmersNotUsed(blockNumber, ommers, blockchain)
      _ <- validateDuplicatedOmmers(ommers)
    } yield ommers
  }

  /**
    * Validates ommers length
    * based on validations stated in section 11.1 of http://paper.gavwood.com/
    *
    * @param ommers         The list of ommers to validate
    * @return ommers if valid, an [[OmmersLengthError]] otherwise
    */
  private def validateOmmersLength(ommers: Seq[BlockHeader]): Either[OmmersError, Seq[BlockHeader]] = {
    if(ommers.length <= OmmerSizeLimit) Right(ommers)
    else Left(OmmersLengthError)
  }

  /**
    * Validates that each ommer's header is valid
    * based on validations stated in section 11.1 of http://paper.gavwood.com/
    *
    * @param ommers         The list of ommers to validate
    * @param blockchain     from where the ommer's parents will be obtained
    * @return ommers if valid, an [[OmmersNotValidError]] otherwise
    */
  private def validateOmmersHeaders(ommers: Seq[BlockHeader], blockchain: Blockchain): Either[OmmersError, Seq[BlockHeader]] = {
    if(ommers.forall(BlockHeaderValidator.validate(_, blockchain).isRight)) Right(ommers)
    else Left(OmmersNotValidError)
  }

  /**
    * Validates that each ommer is not too old and that it is a sibling as one of the current block's ancestors
    * based on validations stated in section 11.1 of http://paper.gavwood.com/
    *
    * @param blockNumber    The number of the block to which the ommers belong
    * @param ommers         The list of ommers to validate
    * @param blockchain     from where the ommer's parents will be obtained
    * @return ommers if valid, an [[OmmersUsedBeforeError]] otherwise
    */
  private def validateOmmersAncestors(blockNumber: BigInt, ommers: Seq[BlockHeader], blockchain: Blockchain): Either[OmmersError, Seq[BlockHeader]] = {
    val ancestors = getAncestors(blockNumber, OmmerGenerationLimit, blockchain)

    val validOmmersAncestors: Seq[BlockHeader] => Boolean = anc =>
      ommers.forall{ ommer =>
        val ommerIsNotAncestor = anc.forall(_.hash != ommer.hash)
        val ommersParentIsAncestor = anc.exists(_.parentHash == ommer.parentHash)
        ommerIsNotAncestor && ommersParentIsAncestor
      }
    if(ancestors.exists(validOmmersAncestors(_))) Right(ommers)
    else Left(OmmersAncestorsError)
  }

  /**
    * Validates that each ommer was not previously used
    * based on validations stated in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    *
    * @param blockNumber    The number of the block to which the ommers belong
    * @param ommers         The list of ommers to validate
    * @param blockchain     from where the ommer's parents will be obtained
    * @return ommers if valid, an [[OmmersUsedBeforeError]] otherwise
    */
  private def validateOmmersNotUsed(blockNumber: BigInt, ommers: Seq[BlockHeader], blockchain: Blockchain): Either[OmmersError, Seq[BlockHeader]] = {
    val usedOmmersHashes = getUsedOmmers(blockNumber, OmmerGenerationLimit, blockchain)

    val allOmmersUnused: Seq[BlockHeader] => Boolean = usedOmmHashes =>
      ommers.intersect(usedOmmHashes).isEmpty
    if(usedOmmersHashes.exists(allOmmersUnused(_))) Right(ommers)
    else Left(OmmersUsedBeforeError)
  }

  /**
    * Validates that there are no duplicated ommers
    * based on validations stated in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    *
    * @param ommers         The list of ommers to validate
    * @return ommers if valid, an [[OmmersDuplicatedError]] otherwise
    */
  private def validateDuplicatedOmmers(ommers: Seq[BlockHeader]): Either[OmmersError, Seq[BlockHeader]] = {
    if(ommers.distinct.length == ommers.length) Right(ommers)
    else Left(OmmersDuplicatedError)
  }

  private def getAncestors(blockNumber: BigInt, numberOfBlocks: Int, blockchain: Blockchain): Option[Seq[BlockHeader]] = {
    val ancestors: Seq[Option[BlockHeader]] = (1 to numberOfBlocks)
      .collect{ case i if blockNumber - i >= 0 => blockNumber - i }
      .map{ num => blockchain.getBlockHeaderByNumber(num) }
    if(ancestors.exists(_.isEmpty)) None
    else Some(ancestors.flatten)
  }

  private def getUsedOmmers(blockNumber: BigInt, numberOfBlocks: Int, blockchain: Blockchain): Option[Seq[BlockHeader]] = {
    val usedOmmers: Seq[Option[Seq[BlockHeader]]] = (1 to numberOfBlocks)
      .collect{ case i if blockNumber - i >= 0 => blockNumber - i }
      .map{ num => blockchain.getBlockByNumber(num).map(_.body.uncleNodesList) }
    if(usedOmmers.exists(_.isEmpty)) None
    else Some(usedOmmers.flatten.flatten)
  }

  sealed trait OmmersError

  object OmmersError {
    case object OmmersLengthError extends OmmersError
    case object OmmersNotValidError extends OmmersError
    case object OmmersUsedBeforeError extends OmmersError
    case object OmmersAncestorsError extends OmmersError
    case object OmmersDuplicatedError extends OmmersError
  }
}
