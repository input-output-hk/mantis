package io.iohk.ethereum.ets.blockchain

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.ethash.validators.EthashBlockHeaderValidator
import io.iohk.ethereum.consensus.validators.{ BlockHeaderError, BlockHeaderValid, BlockHeaderValidatorSkeleton }
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class EthashTestBlockHeaderValidator(blockchainConfig: BlockchainConfig) extends BlockHeaderValidatorSkeleton(blockchainConfig) {
  import EthashBlockHeaderValidator._

  // NOTE the below comment is from before PoW decoupling
  // we need concurrent map since validators can be used from multiple places
  protected val powCaches: java.util.concurrent.ConcurrentMap[Long, PowCacheData] = new java.util.concurrent.ConcurrentHashMap[Long, PowCacheData]()

  protected def difficulty: DifficultyCalculator = DifficultyCalculator(blockchainConfig)

  override def validateEvenMore(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    Right(BlockHeaderValid)

}
