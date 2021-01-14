package io.iohk.ethereum.consensus
package ethash

import java.io.{File, FileInputStream, FileOutputStream}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.consensus.blocks.{PendingBlock, PendingBlockAndState}
import io.iohk.ethereum.consensus.ethash.EthashUtils.ProofOfWork
import io.iohk.ethereum.consensus.ethash.MinerProtocol.{StartMining, StopMining}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.jsonrpc.{EthService, EthMiningService}
import io.iohk.ethereum.jsonrpc.EthMiningService.SubmitHashRateRequest
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.{ByteStringUtils, ByteUtils}
import monix.execution.Scheduler
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

/**
  * Implementation of Ethash CPU mining worker.
  * Could be started by switching configuration flag "consensus.mining-enabled" to true
  */
class EthashMiner(
    blockchain: Blockchain,
    blockCreator: EthashBlockCreator,
    syncController: ActorRef,
    ethService: EthService,
    miningService: EthMiningService
) extends Actor
    with ActorLogging {

  import EthashMiner._

  private implicit val scheduler: Scheduler = Scheduler(context.dispatcher)

  var currentEpoch: Option[Long] = None
  var currentEpochDagSize: Option[Long] = None
  var currentEpochDag: Option[Array[Array[Int]]] = None

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
    val parentBlock = blockchain.getBestBlock()
    val blockNumber = parentBlock.header.number.toLong + 1
    val epoch = EthashUtils.epoch(blockNumber, blockCreator.blockchainConfig.ecip1099BlockNumber.toLong)
    val (dag, dagSize) = calculateDagSize(blockNumber, epoch)

    blockCreator
      .getBlockForMining(parentBlock)
      .map { case PendingBlockAndState(PendingBlock(block, _), _) =>
        val headerHash = crypto.kec256(BlockHeader.getEncodedWithoutNonce(block.header))
        val startTime = System.nanoTime()
        val mineResult =
          mine(headerHash, block.header.difficulty.toLong, dagSize, dag, blockCreator.miningConfig.mineRounds)
        val time = System.nanoTime() - startTime
        //FIXME: consider not reporting hash rate when time delta is zero
        val hashRate = if (time > 0) (mineResult.triedHashes.toLong * 1000000000) / time else Long.MaxValue
        miningService.submitHashRate(SubmitHashRateRequest(hashRate, ByteString("mantis-miner")))
        mineResult match {
          case MiningSuccessful(_, pow, nonce) =>
            log.info(
              s"Mining successful with ${ByteStringUtils.hash2string(pow.mixHash)} and nonce ${ByteStringUtils.hash2string(nonce)}"
            )
            syncController ! SyncProtocol.MinedBlock(
              block.copy(header = block.header.copy(nonce = nonce, mixHash = pow.mixHash))
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
  }

  private def calculateDagSize(blockNumber: Long, epoch: Long): (Array[Array[Int]], Long) = {
    (currentEpoch, currentEpochDag, currentEpochDagSize) match {
      case (Some(`epoch`), Some(dag), Some(dagSize)) => (dag, dagSize)
      case _ =>
        val seed = EthashUtils.seed(blockNumber)
        val dagSize = EthashUtils.dagSize(epoch)
        val dagNumHashes = (dagSize / EthashUtils.HASH_BYTES).toInt
        val dag =
          if (!dagFile(seed).exists()) generateDagAndSaveToFile(epoch, dagNumHashes, seed)
          else {
            val res = loadDagFromFile(seed, dagNumHashes)
            res.failed.foreach { ex =>
              log.error(ex, "Cannot read DAG from file")
            }
            res.getOrElse(generateDagAndSaveToFile(epoch, dagNumHashes, seed))
          }
        currentEpoch = Some(epoch)
        currentEpochDag = Some(dag)
        currentEpochDagSize = Some(dagSize)
        (dag, dagSize)
    }
  }

  private def dagFile(seed: ByteString): File = {
    new File(
      s"${blockCreator.miningConfig.ethashDir}/full-R${EthashUtils.Revision}-${Hex.toHexString(seed.take(8).toArray[Byte])}"
    )
  }

  private def generateDagAndSaveToFile(epoch: Long, dagNumHashes: Int, seed: ByteString): Array[Array[Int]] = {
    // scalastyle:off magic.number
    val file = dagFile(seed)
    if (file.exists()) file.delete()
    file.getParentFile.mkdirs()
    file.createNewFile()

    val outputStream = new FileOutputStream(dagFile(seed).getAbsolutePath)
    outputStream.write(DagFilePrefix.toArray[Byte])

    val cache = EthashUtils.makeCache(epoch, seed)
    val res = new Array[Array[Int]](dagNumHashes)

    (0 until dagNumHashes).foreach { i =>
      val item = EthashUtils.calcDatasetItem(cache, i)
      outputStream.write(ByteUtils.intsToBytes(item, bigEndian = false))
      res(i) = item

      if (i % 100000 == 0) log.info(s"Generating DAG ${((i / dagNumHashes.toDouble) * 100).toInt}%")
    }

    Try(outputStream.close())

    res
  }

  private def loadDagFromFile(seed: ByteString, dagNumHashes: Int): Try[Array[Array[Int]]] = {
    val inputStream = new FileInputStream(dagFile(seed).getAbsolutePath)

    val prefix = new Array[Byte](8)
    if (inputStream.read(prefix) != 8 || ByteString(prefix) != DagFilePrefix) {
      Failure(new RuntimeException("Invalid DAG file prefix"))
    } else {
      val buffer = new Array[Byte](64)
      val res = new Array[Array[Int]](dagNumHashes)
      var index = 0

      while (inputStream.read(buffer) > 0) {
        if (index % 100000 == 0) log.info(s"Loading DAG from file ${((index / res.length.toDouble) * 100).toInt}%")
        res(index) = ByteUtils.bytesToInts(buffer, bigEndian = false)
        index += 1
      }

      Try(inputStream.close())

      if (index == dagNumHashes) Success(res)
      else Failure(new RuntimeException("DAG file ended unexpectedly"))
    }
  }

  private def mine(
      headerHash: Array[Byte],
      difficulty: Long,
      dagSize: Long,
      dag: Array[Array[Int]],
      numRounds: Int
  ): MiningResult = {
    // scalastyle:off magic.number
    val initNonce = BigInt(64, new Random())

    (0 to numRounds).iterator
      .map { n =>
        val nonce = (initNonce + n) % MaxNonce
        val nonceBytes = ByteUtils.padLeft(ByteString(nonce.toUnsignedByteArray), 8)
        val pow = EthashUtils.hashimoto(headerHash, nonceBytes.toArray[Byte], dagSize, dag.apply)
        (EthashUtils.checkDifficulty(difficulty, pow), pow, nonceBytes, n)
      }
      .collectFirst { case (true, pow, nonceBytes, n) => MiningSuccessful(n + 1, pow, nonceBytes) }
      .getOrElse(MiningUnsuccessful(numRounds))
  }

}

object EthashMiner {
  final val BlockForgerDispatcherId = "mantis.async.dispatchers.block-forger"

  private[ethash] def props(
      blockchain: Blockchain,
      blockCreator: EthashBlockCreator,
      syncController: ActorRef,
      ethService: EthService,
      miningService: EthMiningService
  ): Props =
    Props(
      new EthashMiner(blockchain, blockCreator, syncController, ethService, miningService)
    ).withDispatcher(BlockForgerDispatcherId)

  def apply(node: Node): ActorRef = {
    node.consensus match {
      case consensus: EthashConsensus =>
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
          ethService = node.ethService,
          miningService = node.miningService
        )
        node.system.actorOf(minerProps)
      case consensus =>
        wrongConsensusArgument[EthashConsensus](consensus)
    }
  }

  private case object ProcessMining

  // scalastyle:off magic.number
  val MaxNonce: BigInt = BigInt(2).pow(64) - 1

  val DagFilePrefix: ByteString = ByteString(Array(0xfe, 0xca, 0xdd, 0xba, 0xad, 0xde, 0xe1, 0xfe).map(_.toByte))

  sealed trait MiningResult {
    def triedHashes: Int
  }
  case class MiningSuccessful(triedHashes: Int, pow: ProofOfWork, nonce: ByteString) extends MiningResult
  case class MiningUnsuccessful(triedHashes: Int) extends MiningResult

}
