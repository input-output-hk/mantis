package io.iohk.ethereum.consensus.ethash
package validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderPoWError
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidatorSkeleton}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig
import monix.execution.atomic.{Atomic, AtomicAny}

/**
  * A block header validator for Ethash.
  */
class EthashBlockHeaderValidator(blockchainConfig: BlockchainConfig)
    extends BlockHeaderValidatorSkeleton(blockchainConfig) {
  import EthashBlockHeaderValidator._

  // NOTE the below comment is from before PoW decoupling
  // we need atomic since validators can be used from multiple places
  protected val powCaches: AtomicAny[List[PowCacheData]] = Atomic(List.empty[PowCacheData])

  protected def difficulty: DifficultyCalculator = DifficultyCalculator(blockchainConfig)

  def validateEvenMore(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    validatePoW(blockHeader)

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.nonce]] and [[io.iohk.ethereum.domain.BlockHeader.mixHash]] are correct
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderPoWError]] otherwise
    */
  protected def validatePoW(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    import EthashUtils._

    def getPowCacheData(epoch: Long, seed: ByteString): PowCacheData = {
      powCaches.transformAndExtract { cache =>
        cache.find(_.epoch == epoch) match {
          case Some(pcd) => (pcd, cache)
          case None =>
            val data =
              PowCacheData(epoch, cache = EthashUtils.makeCache(epoch, seed), dagSize = EthashUtils.dagSize(epoch))
            (data, (data :: cache).take(MaxPowCaches))
        }
      }
    }

    val epoch = EthashUtils.epoch(blockHeader.number.toLong, blockchainConfig.ecip1099BlockNumber.toLong)
    val seed = EthashUtils.seed(blockHeader.number.toLong)
    val powCacheData = getPowCacheData(epoch, seed)

    val proofOfWork = hashimotoLight(
      crypto.kec256(BlockHeader.getEncodedWithoutNonce(blockHeader)),
      blockHeader.nonce.toArray[Byte],
      powCacheData.dagSize,
      powCacheData.cache
    )

    if (proofOfWork.mixHash == blockHeader.mixHash && checkDifficulty(blockHeader.difficulty.toLong, proofOfWork))
      Right(BlockHeaderValid)
    else Left(HeaderPoWError)
  }
}

object EthashBlockHeaderValidator {
  final val MaxPowCaches: Int = 2 // maximum number of epochs for which PoW cache is stored in memory

  case class PowCacheData(epoch: Long, cache: Array[Int], dagSize: Long)
}
