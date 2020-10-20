package io.iohk.ethereum.consensus.ethash.difficulty

import akka.util.ByteString
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.ethash.difficulty.EthashDifficultyCalculator._
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.utils.{BlockchainConfig, ByteStringUtils}

class EthashDifficultyCalculator(blockchainConfig: BlockchainConfig, grandParentsDataGetter: GrandparentsDataGetter)
    extends DifficultyCalculator {
  import blockchainConfig._

  val DifficultyBoundDivision: Int = 2048
  val FrontierTimestampDiffLimit: Int = -99
  val ExpDifficultyPeriod: Int = 100000
  val MinimumDifficulty: BigInt = 131072
  val ByzantiumRelaxDifficulty: BigInt = 3000000
  val ConstantinopleRelaxDifficulty: BigInt = 5000000

  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parentHeader: BlockHeader): BigInt = {
    lazy val GrandparentsData(grandparentDifficulty, grandparentsTimestampDiff) = grandParentsDataGetter(
      parentHeader.hash
    )
    val parentHeaderDifficulty = if (!parentHeader.hasCheckpoint) parentHeader.difficulty else grandparentDifficulty
    lazy val timestampDiff = if (!parentHeader.hasCheckpoint) {
      blockTimestamp - parentHeader.unixTimestamp
    } else grandparentsTimestampDiff

    val x: BigInt = parentHeaderDifficulty / DifficultyBoundDivision
    val c: BigInt =
      if (blockNumber < homesteadBlockNumber) {
        if (blockTimestamp < parentHeader.unixTimestamp + 13) 1 else -1
      } else if (blockNumber >= byzantiumBlockNumber || blockNumber >= blockchainConfig.atlantisBlockNumber) {
        val parentUncleFactor = if (parentHeader.ommersHash == BlockHeader.EmptyOmmers) 1 else 2
        math.max(parentUncleFactor - (timestampDiff / 9), FrontierTimestampDiffLimit)
      } else {
        math.max(1 - (timestampDiff / 10), FrontierTimestampDiffLimit)
      }

    val extraDifficulty: BigInt =
      if (blockNumber < difficultyBombRemovalBlockNumber) {
        // calculate a fake block number for the ice-age delay
        val fakeBlockNumber: BigInt =
          // https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1234.md
          if (blockNumber >= constantinopleBlockNumber) (blockNumber - ConstantinopleRelaxDifficulty).max(0)
          // https://github.com/ethereum/EIPs/blob/master/EIPS/eip-649.md
          else if (blockNumber >= byzantiumBlockNumber) (blockNumber - ByzantiumRelaxDifficulty).max(0)
          else blockNumber

        val difficultyBombExponent = calculateBombExponent(fakeBlockNumber)
        if (difficultyBombExponent >= 0)
          BigInt(2).pow(difficultyBombExponent)
        else 0
      } else
        0

    val difficultyWithoutBomb = MinimumDifficulty.max(parentHeaderDifficulty + x * c)
    difficultyWithoutBomb + extraDifficulty
  }

  private def calculateBombExponent(blockNumber: BigInt): Int = {
    if (blockNumber < difficultyBombPauseBlockNumber)
      (blockNumber / ExpDifficultyPeriod - 2).toInt
    else if (blockNumber < difficultyBombContinueBlockNumber)
      ((difficultyBombPauseBlockNumber / ExpDifficultyPeriod) - 2).toInt
    else {
      val delay = (difficultyBombContinueBlockNumber - difficultyBombPauseBlockNumber) / ExpDifficultyPeriod
      ((blockNumber / ExpDifficultyPeriod) - delay - 2).toInt
    }
  }
}

object EthashDifficultyCalculator {
  case class GrandparentsData(grandparentDifficulty: BigInt, grandparentsTimestampDiff: Long)
  type GrandparentsDataGetter = ByteString => GrandparentsData

  def grandparentsDataGetterFromBlockchain(blockchain: Blockchain): GrandparentsDataGetter = { hash =>
    // not having parent/grandparent headers means that sync is corrupted and we want to fail fast
    (for {
      parent <- blockchain.getBlockHeaderByHash(hash)
      grandparent <- blockchain.getBlockHeaderByHash(parent.hash)
    } yield GrandparentsData(parent.difficulty, parent.unixTimestamp - grandparent.unixTimestamp))
      .getOrElse(
        throw new RuntimeException(
          s"Blockchain is corrupted - missing parent/grandparent headers for hash ${ByteStringUtils.hash2string(hash)}"
        )
      )
  }
}
