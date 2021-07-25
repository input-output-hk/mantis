package io.iohk.ethereum.consensus.pow
package validators

import akka.util.ByteString

import monix.execution.atomic.Atomic
import monix.execution.atomic.AtomicAny

import io.iohk.ethereum.consensus.validators.BlockHeaderError
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderPoWError
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

/** A block header validator for Ethash.
  */
object EthashBlockHeaderValidator {
  final val MaxPowCaches: Int = 2 // maximum number of epochs for which PoW cache is stored in memory

  case class PowCacheData(epoch: Long, cache: Array[Int], dagSize: Long)

  // NOTE the below comment is from before PoW decoupling
  // we need atomic since validators can be used from multiple places
  protected val powCaches: AtomicAny[List[PowCacheData]] = Atomic(List.empty[PowCacheData])

  /** Validates [[io.iohk.ethereum.domain.BlockHeader.nonce]] and [[io.iohk.ethereum.domain.BlockHeader.mixHash]] are correct
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeaderValid if valid or an BlockHeaderError.HeaderPoWError otherwise
    */
  def validateHeader(
      blockHeader: BlockHeader
  )(implicit blockchainConfig: BlockchainConfig): Either[BlockHeaderError, BlockHeaderValid] = {
    import EthashUtils._

    def getPowCacheData(epoch: Long, seed: ByteString): PowCacheData =
      powCaches.transformAndExtract { cache =>
        cache.find(_.epoch == epoch) match {
          case Some(pcd) => (pcd, cache)
          case None =>
            val data =
              PowCacheData(epoch, cache = EthashUtils.makeCache(epoch, seed), dagSize = EthashUtils.dagSize(epoch))
            (data, (data :: cache).take(MaxPowCaches))
        }
      }

    val epoch =
      EthashUtils.epoch(blockHeader.number.toLong, blockchainConfig.forkBlockNumbers.ecip1099BlockNumber.toLong)
    val seed = EthashUtils.seed(blockHeader.number.toLong, blockchainConfig.forkBlockNumbers.ecip1099BlockNumber.toLong)
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
