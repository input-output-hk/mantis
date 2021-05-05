package io.iohk.ethereum.consensus.pow.miners

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
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
import io.iohk.ethereum.utils.{ByteStringUtils, ByteUtils}
import monix.execution.Scheduler

import scala.util.Random

object KeccakMiner {
  def apply(
      blockCreator: PoWBlockCreator,
      syncController: akka.actor.ActorRef,
      ethMiningService: EthMiningService
  ): Behavior[MinerProtocol] =
    Behaviors.setup(context => new KeccakMiner(context, blockCreator, syncController, ethMiningService))
}

class KeccakMiner(
    context: ActorContext[MinerProtocol],
    blockCreator: PoWBlockCreator,
    syncController: akka.actor.ActorRef,
    ethMiningService: EthMiningService
) extends AbstractBehavior[MinerProtocol](context) {

  private implicit val scheduler: Scheduler = Scheduler(context.executionContext)
  private val log = context.log
  private val MaxNonce: BigInt = BigInt(2).pow(64) - 1 // scalastyle:ignore magic.number

  override def onMessage(msg: MinerProtocol): Behavior[MinerProtocol] = msg match {
    case message @ MinerProtocol.ProcessMining(bestBlock, coordinatorRef) =>
      log.debug("KeccakMiner received message {}", message)
      processMining(bestBlock, coordinatorRef)
      Behaviors.same
  }

  private def processMining(
      bestBlock: Block,
      coordinatorRef: ActorRef[CoordinatorProtocol]
  ): Unit = {
    blockCreator
      .getBlockForMining(bestBlock)
      .map { case PendingBlockAndState(PendingBlock(block, _), _) =>
        val (startTime, mineResult) = doMining(block, blockCreator.miningConfig.mineRounds)

        submiteHashRate(System.nanoTime() - startTime, mineResult)

        mineResult match {
          case MiningSuccessful(_, mixHash, nonce) =>
            log.info(
              "Mining successful with {} and nonce {}",
              ByteStringUtils.hash2string(mixHash),
              ByteStringUtils.hash2string(nonce)
            )

            syncController ! SyncProtocol.MinedBlock(
              block.copy(header = block.header.copy(nonce = nonce, mixHash = mixHash))
            )

            coordinatorRef ! PoWMiningCoordinator.MiningSuccessful
            Behaviors.stopped
          case _ =>
            log.info("Mining unsuccessful")
            coordinatorRef ! PoWMiningCoordinator.MiningUnsuccessful
            Behaviors.stopped
        }
      }
      .onErrorHandle { ex =>
        log.error("Error occurred: ", ex)
        coordinatorRef ! PoWMiningCoordinator.MiningUnsuccessful
        Behaviors.stopped
      }
      .runAsyncAndForget
  }

  private def submiteHashRate(time: Long, mineResult: MiningResult): Unit = {
    val hashRate = if (time > 0) (mineResult.triedHashes.toLong * 1000000000) / time else Long.MaxValue
    ethMiningService.submitHashRate(SubmitHashRateRequest(hashRate, ByteString("mantis-miner")))
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
