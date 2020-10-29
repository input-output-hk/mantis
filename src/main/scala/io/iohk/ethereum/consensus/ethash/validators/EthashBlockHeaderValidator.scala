package io.iohk.ethereum.consensus.ethash
package validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.ethash.difficulty.EthashDifficultyCalculator
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderPoWError
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidatorSkeleton}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

/**
  * A block header validator for Ethash.
  */
class EthashBlockHeaderValidator(blockchainConfig: BlockchainConfig)
    extends BlockHeaderValidatorSkeleton(blockchainConfig) {
  import EthashBlockHeaderValidator._

  // NOTE the below comment is from before PoW decoupling
  // we need concurrent map since validators can be used from multiple places
  protected val powCaches: java.util.concurrent.ConcurrentMap[Long, PowCacheData] =
    new java.util.concurrent.ConcurrentHashMap[Long, PowCacheData]()

  protected def difficulty: DifficultyCalculator = new EthashDifficultyCalculator(blockchainConfig)

  def validateEvenMore(
      blockHeader: BlockHeader,
      parentHeader: BlockHeader
  ): Either[BlockHeaderError, BlockHeaderValid] =
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

    import scala.collection.JavaConverters._

    def getPowCacheData(epoch: Long, seed: ByteString): PowCacheData = {
      Option(powCaches.get(epoch)) match {
        case Some(pcd) => pcd
        case None =>
          val data = new PowCacheData(cache = EthashUtils.makeCache(epoch, seed), dagSize = EthashUtils.dagSize(epoch))

          val keys = powCaches.keySet().asScala
          val keysToRemove = keys.toSeq.sorted.take(keys.size - MaxPowCaches + 1)
          keysToRemove.foreach(powCaches.remove)

          powCaches.put(epoch, data)

          data
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

  class PowCacheData(val cache: Array[Int], val dagSize: Long)
}
