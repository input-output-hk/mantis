package io.iohk.ethereum.consensus.pow.miners

import akka.util.ByteString

import monix.execution.CancelableFuture
import monix.execution.Scheduler

import scala.util.Random

import io.iohk.ethereum.consensus.blocks.PendingBlock
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.consensus.pow.KeccakCalculation
import io.iohk.ethereum.consensus.pow.PoWBlockCreator
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol.MiningResult
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol.MiningSuccessful
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol.MiningUnsuccessful
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.jsonrpc.EthMiningService
import io.iohk.ethereum.utils.BigIntExtensionMethods.BigIntAsUnsigned
import io.iohk.ethereum.utils.ByteUtils
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.utils.BlockchainConfig

class KeccakMiner(
    blockCreator: PoWBlockCreator,
    syncController: akka.actor.ActorRef,
    ethMiningService: EthMiningService
)(implicit scheduler: Scheduler)
    extends Miner
    with Logger {

  import KeccakMiner._

  def processMining(
      bestBlock: Block
  )(implicit blockchainConfig: BlockchainConfig): CancelableFuture[CoordinatorProtocol] = {
    log.debug("Starting mining with parent block {}", bestBlock.number)
    blockCreator
      .getBlockForMining(bestBlock)
      .map { case PendingBlockAndState(PendingBlock(block, _), _) =>
        val (startTime, miningResult) = doMining(block, blockCreator.miningConfig.mineRounds)

        submitHashRate(ethMiningService, System.nanoTime() - startTime, miningResult)
        handleMiningResult(miningResult, syncController, block)
      }
      .onErrorHandle { ex =>
        log.error("Error occurred while mining: ", ex)
        PoWMiningCoordinator.MiningUnsuccessful
      }
      .runToFuture
  }

  private def doMining(block: Block, numRounds: Int): (Long, MiningResult) = {
    val rlpEncodedHeader = BlockHeader.getEncodedWithoutNonce(block.header)
    val initNonce = BigInt(64, new Random()) // scalastyle:ignore magic.number
    val startTime = System.nanoTime()

    val mined = (0 to numRounds).iterator
      .map { round =>
        val nonce = (initNonce + round) % MaxNonce
        val difficulty = block.header.difficulty
        val hash = KeccakCalculation.hash(rlpEncodedHeader, nonce)
        (KeccakCalculation.isMixHashValid(hash.mixHash, difficulty), hash, nonce, round)
      }
      .collectFirst { case (true, hash, nonce, n) =>
        val nonceBytes = ByteUtils.padLeft(ByteString(nonce.toUnsignedByteArray), 8)
        MiningSuccessful(n + 1, ByteString(hash.mixHash), nonceBytes)
      }
      .getOrElse(MiningUnsuccessful(numRounds))

    (startTime, mined)
  }
}

object KeccakMiner {
  val MaxNonce: BigInt = BigInt(2).pow(64) - 1 // scalastyle:ignore magic.number
}
