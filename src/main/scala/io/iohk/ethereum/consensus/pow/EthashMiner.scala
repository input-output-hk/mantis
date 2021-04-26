package io.iohk.ethereum.consensus
package pow

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.consensus.blocks.{PendingBlock, PendingBlockAndState}
import io.iohk.ethereum.consensus.pow.MinerProtocol.{StartMining, StopMining}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain}
import io.iohk.ethereum.jsonrpc.EthMiningService.SubmitHashRateRequest
import io.iohk.ethereum.jsonrpc.{EthInfoService, EthMiningService}
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.{ByteStringUtils, ByteUtils}
import monix.execution.Scheduler

import scala.concurrent.duration._
import scala.util.Random

/**
  * Implementation of Ethash CPU mining worker.
  * Could be started by switching configuration flag "consensus.mining-enabled" to true
  */
class EthashMiner(
    blockchain: Blockchain,
    blockCreator: EthashBlockCreator,
    syncController: ActorRef,
    ethMiningService: EthMiningService,
    ecip1049BlockNumber: Option[BigInt]
) extends Actor
    with ActorLogging {

  import EthashMiner._

  private implicit val scheduler: Scheduler = Scheduler(context.dispatcher)

  private val dagManager = new EthashDAGManager(blockCreator)

  override def receive: Receive = stopped

  def stopped: Receive = {
    case StartMining =>
      context become started
      self ! ProcessMining
    case ProcessMining => // nothing
  }

  def started: Receive = {
    case StopMining => context become stopped
    case ProcessMining => processMining()
  }

  def processMining(): Unit = {
    blockchain.getBestBlock() match {
      case Some(blockValue) =>
        blockCreator
          .getBlockForMining(blockValue)
          .map { case PendingBlockAndState(PendingBlock(block, _), _) =>
            val blockNumber = block.header.number
            val (startTime, mineResult) =
              // ECIP-1049 //TODO refactoring in ETCM-759 to remove the if clause
              if (isKeccak(blockNumber)) doKeccakMining(block, blockCreator.miningConfig.mineRounds)
              else doEthashMining(blockNumber.toLong, block)

            val time = System.nanoTime() - startTime
            //FIXME: consider not reporting hash rate when time delta is zero
            val hashRate = if (time > 0) (mineResult.triedHashes.toLong * 1000000000) / time else Long.MaxValue
            ethMiningService.submitHashRate(SubmitHashRateRequest(hashRate, ByteString("mantis-miner")))
            mineResult match {
              case MiningSuccessful(_, mixHash, nonce) =>
                log.info(
                  s"Mining successful with ${ByteStringUtils.hash2string(mixHash)} and nonce ${ByteStringUtils.hash2string(nonce)}"
                )
                syncController ! SyncProtocol.MinedBlock(
                  block.copy(header = block.header.copy(nonce = nonce, mixHash = mixHash))
                )
              case _ => log.info("Mining unsuccessful")
            }
            self ! ProcessMining
          }
          .onErrorHandle { ex =>
            log.error(ex, "Unable to get block for mining")
            context.system.scheduler.scheduleOnce(10.seconds, self, ProcessMining)
          }
          .runAsyncAndForget
      case None =>
        log.error("Unable to get block for mining, getBestBlock() returned None")
        context.system.scheduler.scheduleOnce(10.seconds, self, ProcessMining)
    }
  }

  private def isKeccak(currentBlockNumber: BigInt): Boolean = {
    ecip1049BlockNumber match {
      case None => false
      case Some(blockNumber) => currentBlockNumber >= blockNumber
    }
  }

  private def doEthashMining(blockNumber: Long, block: Block): (Long, MiningResult) = {
    val epoch =
      EthashUtils.epoch(blockNumber, blockCreator.blockchainConfig.ecip1099BlockNumber.toLong)
    val (dag, dagSize) =
      dagManager.calculateDagSize(blockNumber, epoch, blockCreator.blockchainConfig.ecip1099BlockNumber.toLong)
    val headerHash = crypto.kec256(BlockHeader.getEncodedWithoutNonce(block.header))
    val startTime = System.nanoTime()
    val mineResult =
      mineEthah(headerHash, block.header.difficulty.toLong, dagSize, dag, blockCreator.miningConfig.mineRounds)
    (startTime, mineResult)
  }

  private def doKeccakMining(block: Block, numRounds: Int): (Long, MiningResult) = {
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
        val nonceBytes = ByteString(ByteUtils.bigIntToUnsignedByteArray(nonce))
        MiningSuccessful(n + 1, ByteString(hash.mixHash), nonceBytes)
      }
      .getOrElse(MiningUnsuccessful(numRounds))

    (startTime, mined)
  }

  private def mineEthah(
      headerHash: Array[Byte],
      difficulty: Long,
      dagSize: Long,
      dag: Array[Array[Int]],
      numRounds: Int
  ): MiningResult = {
    val initNonce = BigInt(64, new Random()) // scalastyle:ignore magic.number

    (0 to numRounds).iterator
      .map { n =>
        val nonce = (initNonce + n) % MaxNonce
        val nonceBytes = ByteUtils.padLeft(ByteString(nonce.toUnsignedByteArray), 8)
        val pow = EthashUtils.hashimoto(headerHash, nonceBytes.toArray[Byte], dagSize, dag.apply)
        (EthashUtils.checkDifficulty(difficulty, pow), pow, nonceBytes, n)
      }
      .collectFirst { case (true, pow, nonceBytes, n) => MiningSuccessful(n + 1, pow.mixHash, nonceBytes) }
      .getOrElse(MiningUnsuccessful(numRounds))
  }
}

object EthashMiner {
  final val BlockForgerDispatcherId = "mantis.async.dispatchers.block-forger"

  private[pow] def props(
      blockchain: Blockchain,
      blockCreator: EthashBlockCreator,
      syncController: ActorRef,
      ethInfoService: EthInfoService,
      ethMiningService: EthMiningService,
      ecip1049BlockNumber: Option[BigInt]
  ): Props =
    Props(
      new EthashMiner(blockchain, blockCreator, syncController, ethMiningService, ecip1049BlockNumber)
    ).withDispatcher(BlockForgerDispatcherId)

  def apply(node: Node): ActorRef = {
    node.consensus match {
      case consensus: PoWConsensus =>
        val blockCreator = new EthashBlockCreator(
          pendingTransactionsManager = node.pendingTransactionsManager,
          getTransactionFromPoolTimeout = node.txPoolConfig.getTransactionFromPoolTimeout,
          consensus = consensus,
          ommersPool = node.ommersPool
        )
        val minerProps = props(
          blockchain = node.blockchain,
          blockCreator = blockCreator,
          syncController = node.syncController,
          ethInfoService = node.ethInfoService,
          ethMiningService = node.ethMiningService,
          ecip1049BlockNumber = node.blockchainConfig.ecip1049BlockNumber
        )
        node.system.actorOf(minerProps)
      case consensus =>
        wrongConsensusArgument[PoWConsensus](consensus)
    }
  }

  private case object ProcessMining

  // scalastyle:off magic.number
  val MaxNonce: BigInt = BigInt(2).pow(64) - 1

  val DagFilePrefix: ByteString = ByteString(Array(0xfe, 0xca, 0xdd, 0xba, 0xad, 0xde, 0xe1, 0xfe).map(_.toByte))

  sealed trait MiningResult {
    def triedHashes: Int
  }

  case class MiningSuccessful(triedHashes: Int, mixHash: ByteString, nonce: ByteString) extends MiningResult
  case class MiningUnsuccessful(triedHashes: Int) extends MiningResult

}
