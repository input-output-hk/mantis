package io.iohk.ethereum.consensus.ethash
package validators

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.validators.BlockHeaderError.HeaderPoWError
import io.iohk.ethereum.validators.BlockHeaderValidatorImpl.PowCacheData
import io.iohk.ethereum.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidator}

// NOTE Copied parts from [[io.iohk.ethereum.validators.BlockHeaderValidatorImpl]]
class BlockHeaderValidatorImpl(blockchainConfig: BlockchainConfig) extends BlockHeaderValidator {
  import BlockHeaderValidatorImpl._

  // NOTE This is code from before PoW decoupling
  // we need concurrent map since validators can be used from multiple places
  val powCaches: java.util.Map[Long, PowCacheData] = new java.util.concurrent.ConcurrentHashMap[Long, PowCacheData]()

  def validate(
    blockHeader: BlockHeader,
    getBlockHeaderByHash: ByteString ⇒ Option[BlockHeader]
  ): Either[BlockHeaderError, BlockHeaderValid] = {

    for {
      _ <- validatePoW(blockHeader)
    } yield BlockHeaderValid
  }

  /**
   * Validates [[io.iohk.ethereum.domain.BlockHeader.nonce]] and [[io.iohk.ethereum.domain.BlockHeader.mixHash]] are correct
   * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
   *
   * @param blockHeader BlockHeader to validate.
   * @return BlockHeader if valid, an [[io.iohk.ethereum.validators.BlockHeaderError.HeaderPoWError]] otherwise
   */
  private def validatePoW(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    import Ethash._
    import scala.collection.JavaConverters._

    def getPowCacheData(epoch: Long): PowCacheData = {
      if (epoch == 0) BlockHeaderValidatorImpl.epoch0PowCache else
        Option(powCaches.get(epoch)) match {
          case Some(pcd) => pcd
          case None =>
            val data = new PowCacheData(
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

    if (proofOfWork.mixHash == blockHeader.mixHash && checkDifficulty(blockHeader.difficulty.toLong, proofOfWork)) Right(BlockHeaderValid)
    else Left(HeaderPoWError)
  }
}

object BlockHeaderValidatorImpl {
  val MaxPowCaches: Int = 2 // maximum number of epochs for which PoW cache is stored in memory

  // NOTE The below FIXME is from before PoW decoupling.

  // FIXME [EC-331]: this is used to speed up ETS Blockchain tests. All blocks in those tests have low numbers (1, 2, 3 ...)
  // so keeping the cache for epoch 0 avoids recalculating it for each individual test. The difference in test runtime
  // can be dramatic - full suite: 26 hours vs 21 minutes on same machine
  // It might be desirable to find a better solution for this - one that doesn't keep this cache unnecessarily
  lazy val epoch0PowCache = new PowCacheData(
    cache = Ethash.makeCache(0),
    dagSize = Ethash.dagSize(0))
}
