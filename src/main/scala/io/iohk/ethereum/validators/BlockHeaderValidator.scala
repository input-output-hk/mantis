package io.iohk.ethereum.validators

import io.iohk.ethereum.consensus.Ethash
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.{BlockHeader, Blockchain, DifficultyCalculator}
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig}

trait BlockHeaderValidator {
  def validate(blockHeader: BlockHeader, blockchain: Blockchain): Either[BlockHeaderError, BlockHeader]
  def validatePreImport(blockHeader: BlockHeader, blockchain: Blockchain): Either[BlockHeaderError, BlockHeader]
}

object BlockHeaderValidatorImpl {
  val MaxExtraDataSize: Int = 32
  val GasLimitBoundDivisor: Int = 1024
  val MinGasLimit: BigInt = 5000 //Although the paper states this value is 125000, on the different clients 5000 is used
  val MaxGasLimit = Long.MaxValue // max gasLimit is equal 2^63-1 according to EIP106
  val MaxPowCaches: Int = 2 // maximum number of epochs for which PoW cache is stored in memory

  case class PowCacheData(cache: Array[Int], dagSize: Long)
}

class BlockHeaderValidatorImpl(blockchainConfig: BlockchainConfig) extends BlockHeaderValidator {

  import BlockHeaderValidatorImpl._
  import BlockHeaderError._

  // we need concurrent map since validators can be used from multiple places
  val powCaches: java.util.Map[Long, PowCacheData] = new java.util.concurrent.ConcurrentHashMap[Long, PowCacheData]()

  val difficulty = new DifficultyCalculator(blockchainConfig)

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

  /** TODO: implement EC-319
    * should consist of validating PoW and that difficulty is in a certain range (parent unknown)
    */
  def validatePreImport(blockHeader: BlockHeader, blockchain: Blockchain): Either[BlockHeaderError, BlockHeader] = {
    Right(blockHeader)
  }

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.extraData]] length
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[HeaderExtraDataError]] otherwise
    */
  private def validateExtraData(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {

    def validateDaoForkExtraData(blockHeader: BlockHeader, daoForkConfig: DaoForkConfig): Either[BlockHeaderError, BlockHeader] =
      (daoForkConfig requiresExtraData blockHeader.number, daoForkConfig.blockExtraData) match {
        case (false, _) =>
          Right(blockHeader)
        case (true, Some(forkExtraData)) if blockHeader.extraData == forkExtraData =>
          Right(blockHeader)
        case _ =>
          Left(DaoHeaderExtraDataError)
      }

    if (blockHeader.extraData.length <= MaxExtraDataSize) {
      import blockchainConfig._
      daoForkConfig.map(c => validateDaoForkExtraData(blockHeader, c)).getOrElse(Right(blockHeader))
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
    * EIP106(https://github.com/ethereum/EIPs/issues/106) adds additional validation of maximum value for gasLimit.
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderGasLimitError]] otherwise
    */
  private def validateGasLimit(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {

    if (blockHeader.gasLimit > MaxGasLimit && blockHeader.number >= blockchainConfig.eip106BlockNumber)
      Left(HeaderGasLimitError)
    else {
      val gasLimitDiff = (blockHeader.gasLimit - parentHeader.gasLimit).abs
      val gasLimitDiffLimit = parentHeader.gasLimit / GasLimitBoundDivisor
      if (gasLimitDiff < gasLimitDiffLimit && blockHeader.gasLimit >= MinGasLimit)
        Right(blockHeader)
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
  private def validatePoW(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    import Ethash._
    import scala.collection.JavaConverters._

    def getPowCacheData(epoch: Long): PowCacheData = {
      Option(powCaches.get(epoch)) match {
        case Some(pcd) => pcd
        case None =>
          val data = PowCacheData(
            cache = Ethash.makeCache(epoch),
            dagSize = Ethash.dagSize(epoch))

          val keys = powCaches.keySet().asScala
          val keysToRemove = keys.toSeq.sorted.take(keys.size - MaxPowCaches + 1)
          keysToRemove.foreach(powCaches.remove)

          powCaches.put(epoch, data)

          data
      }
    }

    val powCacheData = getPowCacheData(epoch(blockHeader.number.toLong))

    val proofOfWork = hashimotoLight(crypto.kec256(BlockHeader.getEncodedWithoutNonce(blockHeader)),
      blockHeader.nonce.toArray[Byte], powCacheData.dagSize, powCacheData.cache)

    if (proofOfWork.mixHash == blockHeader.mixHash && checkDifficulty(blockHeader, proofOfWork)) Right(blockHeader)
    else Left(HeaderPoWError)
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
  case object DaoHeaderExtraDataError extends BlockHeaderError
  case object HeaderTimestampError extends BlockHeaderError
  case object HeaderDifficultyError extends BlockHeaderError
  case object HeaderGasUsedError extends BlockHeaderError
  case object HeaderGasLimitError extends BlockHeaderError
  case object HeaderNumberError extends BlockHeaderError
  case object HeaderPoWError extends BlockHeaderError
}
