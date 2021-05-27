package io.iohk.ethereum.consensus.pow.miners

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.consensus.blocks.{PendingBlock, PendingBlockAndState}
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol.{MiningResult, MiningSuccessful, MiningUnsuccessful}
import io.iohk.ethereum.consensus.pow.{KeccakCalculation, PoWBlockCreator, PoWMiningCoordinator}
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.jsonrpc.EthMiningService
import io.iohk.ethereum.jsonrpc.EthMiningService.SubmitHashRateRequest
import io.iohk.ethereum.utils.BigIntExtensionMethods.BigIntAsUnsigned
import io.iohk.ethereum.utils.{ByteStringUtils, ByteUtils, Logger}
import monix.execution.{CancelableFuture, Scheduler}

import scala.util.Random

class KeccakMiner(
    blockCreator: PoWBlockCreator,
    syncController: akka.actor.ActorRef,
    ethMiningService: EthMiningService
)(implicit scheduler: Scheduler)
    extends Miner
    with Logger {

  import KeccakMiner._

  def processMining(bestBlock: Block): CancelableFuture[CoordinatorProtocol] = {
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
