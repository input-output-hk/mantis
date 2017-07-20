package io.iohk.ethereum.validators

import akka.util.ByteString
import io.iohk.ethereum.crypto.{kec256, kec512}
import io.iohk.ethereum.domain.{BlockHeader, Blockchain, DifficultyCalculator}
import io.iohk.ethereum.utils.BlockchainConfig
import org.spongycastle.util.encoders.Hex

trait BlockHeaderValidator {

  def validate(blockHeader: BlockHeader, blockchain: Blockchain): Either[BlockHeaderError, BlockHeader]

}

class BlockHeaderValidatorImpl(blockchainConfig: BlockchainConfig) extends BlockHeaderValidator {

  val MaxExtraDataSize: Int = 32
  val GasLimitBoundDivisor: Int = 1024
  val MinGasLimit: BigInt = 5000 //Although the paper states this value is 125000, on the different clients 5000 is used
  val difficulty = new DifficultyCalculator(blockchainConfig)
  import BlockHeaderError._

  /** This method allows validate a BlockHeader (stated on
    * section 4.4.4 of http://paper.gavwood.com/).
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    */
  def validate(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    for {
      _ <- validateExtraData(blockHeader)
      _ <- validateTimestamp(blockHeader, parentHeader)
      _ <- validateDifficulty(blockHeader, parentHeader)
      _ <- validateGasUsed(blockHeader)
      _ <- validateGasLimit(blockHeader, parentHeader)
      _ <- validateNumber(blockHeader, parentHeader)
      _ <- validatePoW(blockHeader)
    } yield blockHeader
  }

  /** This method allows validate a BlockHeader (stated on
    * section 4.4.4 of http://paper.gavwood.com/).
    *
    * @param blockHeader BlockHeader to validate.
    * @param blockchain from where the header of the parent of the block will be fetched.
    */
  def validate(blockHeader: BlockHeader, blockchain: Blockchain): Either[BlockHeaderError, BlockHeader] = {
    for {
      blockHeaderParent <- obtainBlockParentHeader(blockHeader, blockchain)
      _ <- validate(blockHeader, blockHeaderParent)
    } yield blockHeader
  }

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.extraData]] length
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[HeaderExtraDataError]] otherwise
    */
  private def validateExtraData(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] =
    if(blockHeader.extraData.length <= MaxExtraDataSize) Right(blockHeader)
    else Left(HeaderExtraDataError)

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.unixTimestamp]] is greater than the one of its parent
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderTimestampError]] otherwise
    */
  private def validateTimestamp(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] =
    if(blockHeader.unixTimestamp > parentHeader.unixTimestamp) Right(blockHeader)
    else Left(HeaderTimestampError)

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.difficulty]] is correct
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderDifficultyError]] otherwise
    */
  private def validateDifficulty(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] =
    if (difficulty.calculateDifficulty(blockHeader.number, blockHeader.unixTimestamp, parentHeader) == blockHeader.difficulty) Right(blockHeader)
    else Left(HeaderDifficultyError)

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.gasUsed]] is not greater than [[io.iohk.ethereum.domain.BlockHeader.gasLimit]]
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[HeaderGasUsedError]] otherwise
    */
  private def validateGasUsed(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] =
    if(blockHeader.gasUsed<=blockHeader.gasLimit) Right(blockHeader)
    else Left(HeaderGasUsedError)

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.gasLimit]] follows the restrictions based on its parent gasLimit
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderGasLimitError]] otherwise
    */
  private def validateGasLimit(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    val gasLimitDiff = (blockHeader.gasLimit - parentHeader.gasLimit).abs
    val gasLimitDiffLimit = parentHeader.gasLimit / GasLimitBoundDivisor
    if(gasLimitDiff < gasLimitDiffLimit && blockHeader.gasLimit >= MinGasLimit) Right(blockHeader)
    else Left(HeaderGasLimitError)
  }

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.number]] is the next one after its parents number
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderNumberError]] otherwise
    */
  private def validateNumber(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] =
    if(blockHeader.number == parentHeader.number + 1) Right(blockHeader)
    else Left(HeaderNumberError)

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.nonce]] and [[io.iohk.ethereum.domain.BlockHeader.mixHash]] are correct
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[HeaderPoWError]] otherwise
    */
  //FIXME: Simple PoW validation without using DAG [EC-88]
  private def validatePoW(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    val powBoundary = BigInt(2).pow(256) / blockHeader.difficulty
    val powValue = BigInt(1, calculatePoWValue(blockHeader).toArray)
    if(powValue <= powBoundary) Right(blockHeader)
    else Left(HeaderPoWError)
  }

  private def calculatePoWValue(blockHeader: BlockHeader): ByteString = {
    val nonceReverted = blockHeader.nonce.reverse
    val hashBlockWithoutNonce = kec256(BlockHeader.getEncodedWithoutNonce(blockHeader))
    val seedHash = kec512(hashBlockWithoutNonce ++ nonceReverted)

    ByteString(kec256(seedHash ++ blockHeader.mixHash))
  }

  /**
    * Retrieves the header of the parent of a block from the Blockchain, if it exists.
    *
    * @param blockHeader BlockHeader whose parent we want to fetch.
    * @param blockchain where the header of the parent of the block will be fetched.
    * @return the BlockHeader of the parent if it exists, an [[HeaderParentNotFoundError]] otherwise
    */
  private def obtainBlockParentHeader(blockHeader: BlockHeader,
                                      blockchain: Blockchain): Either[BlockHeaderError, BlockHeader] = {
    blockchain.getBlockHeaderByHash(blockHeader.parentHash) match {
      case Some(blockParentHeader) => Right(blockParentHeader)
      case None => Left(HeaderParentNotFoundError)
    }
  }
}

sealed trait BlockHeaderError

object BlockHeaderError {
  case object HeaderParentNotFoundError extends BlockHeaderError
  case object HeaderExtraDataError extends BlockHeaderError
  case object HeaderTimestampError extends BlockHeaderError
  case object HeaderDifficultyError extends BlockHeaderError
  case object HeaderGasUsedError extends BlockHeaderError
  case object HeaderGasLimitError extends BlockHeaderError
  case object HeaderNumberError extends BlockHeaderError
  case object HeaderPoWError extends BlockHeaderError
}
