package io.iohk.ethereum.consensus.pow.miners

import akka.actor.{ActorRef => ClassicActorRef}
import akka.util.ByteString

import monix.execution.CancelableFuture
import monix.execution.Scheduler

import scala.util.Random

import io.iohk.ethereum.consensus.blocks.PendingBlock
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.consensus.pow.EthashUtils
import io.iohk.ethereum.consensus.pow.PoWBlockCreator
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol._
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.jsonrpc.EthMiningService
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.ByteUtils
import io.iohk.ethereum.utils.Logger

/** Implementation of Ethash CPU mining worker.
  * Could be started by switching configuration flag "mining.mining-enabled" to true
  * Implementation explanation at https://eth.wiki/concepts/ethash/ethash
  */
class EthashMiner(
    dagManager: EthashDAGManager,
    blockCreator: PoWBlockCreator,
    syncController: ClassicActorRef,
    ethMiningService: EthMiningService
)(implicit scheduler: Scheduler)
    extends Miner
    with Logger {

  import EthashMiner._

  def processMining(bestBlock: Block): CancelableFuture[CoordinatorProtocol] = {
    log.debug("Starting mining with parent block {}", bestBlock.number)
    blockCreator
      .getBlockForMining(bestBlock)
      .map { case PendingBlockAndState(PendingBlock(block, _), _) =>
        val blockNumber = block.header.number
        val (startTime, miningResult) = doMining(blockNumber.toLong, block)

        submitHashRate(ethMiningService, System.nanoTime() - startTime, miningResult)
        handleMiningResult(miningResult, syncController, block)
      }
      .onErrorHandle { ex =>
        log.error("Error occurred while mining: ", ex)
        PoWMiningCoordinator.MiningUnsuccessful
      }
      .runToFuture
  }

  private def doMining(blockNumber: Long, block: Block): (Long, MiningResult) = {
    val epoch =
      EthashUtils.epoch(blockNumber, blockCreator.blockchainConfig.forkBlockNumbers.ecip1099BlockNumber.toLong)
    val (dag, dagSize) = dagManager.calculateDagSize(blockNumber, epoch)
    val headerHash = crypto.kec256(BlockHeader.getEncodedWithoutNonce(block.header))
    val startTime = System.nanoTime()
    val mineResult =
      mineEthash(headerHash, block.header.difficulty.toLong, dagSize, dag, blockCreator.miningConfig.mineRounds)
    (startTime, mineResult)
  }

  private def mineEthash(
      headerHash: Array[Byte],
      difficulty: Long,
      dagSize: Long,
      dag: Array[Array[Int]],
      numRounds: Int
  ): MiningResult = {
    val initNonce = BigInt(NumBits, new Random())

    (0 to numRounds).iterator
      .map { round =>
        val nonce = (initNonce + round) % MaxNounce
        val nonceBytes = ByteUtils.padLeft(ByteString(nonce.toUnsignedByteArray), 8)
        val pow = EthashUtils.hashimoto(headerHash, nonceBytes.toArray[Byte], dagSize, dag.apply)
        (EthashUtils.checkDifficulty(difficulty, pow), pow, nonceBytes, round)
      }
      .collectFirst { case (true, pow, nonceBytes, n) => MiningSuccessful(n + 1, pow.mixHash, nonceBytes) }
      .getOrElse(MiningUnsuccessful(numRounds))
  }
}

object EthashMiner {
  final val BlockForgerDispatcherId = "mantis.async.dispatchers.block-forger"

  // scalastyle:off magic.number
  final val MaxNounce: BigInt = BigInt(2).pow(64) - 1

  final val NumBits: Int = 64

  final val DagFilePrefix: ByteString = ByteString(Array(0xfe, 0xca, 0xdd, 0xba, 0xad, 0xde, 0xe1, 0xfe).map(_.toByte))
}
