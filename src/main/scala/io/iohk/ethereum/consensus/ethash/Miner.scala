package io.iohk.ethereum.consensus
package ethash

import java.io.{File, FileInputStream, FileOutputStream}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.sync.RegularSync
import io.iohk.ethereum.consensus.ethash.Ethash.ProofOfWork
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain}
import io.iohk.ethereum.jsonrpc.EthService
import io.iohk.ethereum.jsonrpc.EthService.SubmitHashRateRequest
import io.iohk.ethereum.mining.{BlockGenerator, PendingBlock}
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.ByteUtils
import org.spongycastle.util.encoders.Hex

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}
import io.iohk.ethereum.utils.BigIntExtensionMethods._

class Miner(
  blockchain: Blockchain,
  blockGenerator: BlockGenerator,
  ommersPool: ActorRef,
  pendingTransactionsManager: ActorRef,
  syncController: ActorRef,
  miningConfig: MiningConfig,
  ethService: EthService,
  consensus: Consensus
) extends Actor with ActorLogging {

  import Miner._
  import akka.pattern.ask

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
    val epoch = Ethash.epoch(parentBlock.header.number.toLong + 1)

    val (dag, dagSize) = (currentEpoch, currentEpochDag, currentEpochDagSize) match {
      case (Some(`epoch`), Some(dag), Some(dagSize)) => (dag, dagSize)
      case _ =>
        val seed = Ethash.seed(epoch)
        val dagSize = Ethash.dagSize(epoch)
        val dagNumHashes = (dagSize / Ethash.HASH_BYTES).toInt
        val dag =
          if (!dagFile(seed).exists()) generateDagAndSaveToFile(epoch, dagNumHashes, seed)
          else {
            val res = loadDagFromFile(seed, dagNumHashes)
            res.failed.foreach { ex => log.error(ex, "Cannot read DAG from file") }
            res.getOrElse(generateDagAndSaveToFile(epoch, dagNumHashes, seed))
          }

        currentEpoch = Some(epoch)
        currentEpochDag = Some(dag)
        currentEpochDagSize = Some(dagSize)
        (dag, dagSize)
    }

    getBlockForMining(parentBlock) onComplete {
      case Success(PendingBlock(block, _)) =>
        val headerHash = crypto.kec256(BlockHeader.getEncodedWithoutNonce(block.header))
        val startTime = System.currentTimeMillis()
        val mineResult = mine(headerHash, block.header.difficulty.toLong, dagSize, dag, miningConfig.mineRounds)
        val time = System.currentTimeMillis() - startTime
        val hashRate = (mineResult.triedHashes * 1000) / time
        ethService.submitHashRate(SubmitHashRateRequest(hashRate, ByteString("mantis-miner")))
        mineResult match {
          case MiningSuccessful(_, pow, nonce) =>
            syncController ! RegularSync.MinedBlock(block.copy(header = block.header.copy(nonce = nonce, mixHash = pow.mixHash)))
          case _ => // nothing
        }
        self ! ProcessMining

      case Failure(ex) =>
        log.error(ex, "Unable to get block for mining")
        context.system.scheduler.scheduleOnce(10.seconds, self, ProcessMining)
    }
  }

  private def dagFile(seed: ByteString): File = {
    new File(s"${miningConfig.ethashDir}/full-R${Ethash.Revision}-${Hex.toHexString(seed.take(8).toArray[Byte])}")
  }

  private def generateDagAndSaveToFile(epoch: Long, dagNumHashes: Int, seed: ByteString): Array[Array[Int]] = {
    // scalastyle:off magic.number

    val file = dagFile(seed)
    if (file.exists()) file.delete()
    file.getParentFile.mkdirs()
    file.createNewFile()

    val outputStream = new FileOutputStream(dagFile(seed).getAbsolutePath)
    outputStream.write(DagFilePrefix.toArray[Byte])

    val cache = Ethash.makeCache(epoch)
    val res = new Array[Array[Int]](dagNumHashes)

    (0 until dagNumHashes).foreach { i =>
      val item = Ethash.calcDatasetItem(cache, i)
      outputStream.write(ByteUtils.intsToBytes(item))
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
        res(index) = ByteUtils.bytesToInts(buffer)
        index += 1
      }

      Try(inputStream.close())

      if (index == dagNumHashes) Success(res)
      else Failure(new RuntimeException("DAG file ended unexpectedly"))
    }
  }

  private def mine(headerHash: Array[Byte], difficulty: Long, dagSize: Long, dag: Array[Array[Int]], numRounds: Int): MiningResult = {
    // scalastyle:off magic.number
    val initNonce = BigInt(64, new Random())

    (0 to numRounds).toStream.map { n =>
      val nonce = (initNonce + n) % MaxNonce
      val nonceBytes = ByteUtils.padLeft(ByteString(nonce.toUnsignedByteArray), 8)
      val pow = Ethash.hashimoto(headerHash, nonceBytes.toArray[Byte], dagSize, dag.apply)
      (Ethash.checkDifficulty(difficulty, pow), pow, nonceBytes, n)
    }
      .collectFirst { case (true, pow, nonceBytes, n) => MiningSuccessful(n + 1, pow, nonceBytes) }
      .getOrElse(MiningUnsuccessful(numRounds))
  }

  private def getBlockForMining(parentBlock: Block): Future[PendingBlock] = {
    getOmmersFromPool(parentBlock.header.number + 1).zip(getTransactionsFromPool).flatMap { case (ommers, pendingTxs) =>
      blockGenerator.generateBlockForMining(parentBlock, pendingTxs.pendingTransactions.map(_.stx), ommers.headers, miningConfig.coinbase) match {
        case Right(pb) => Future.successful(pb)
        case Left(err) => Future.failed(new RuntimeException(s"Error while generating block for mining: $err"))
      }
    }
  }

  private def getOmmersFromPool(blockNumber: BigInt) = {
    implicit val timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

    (ommersPool ? OmmersPool.GetOmmers(blockNumber)).mapTo[OmmersPool.Ommers]
      .recover { case ex =>
        log.error(ex, "Failed to get ommers, mining block with empty ommers list")
        OmmersPool.Ommers(Nil)
      }
  }

  private def getTransactionsFromPool = {
    implicit val timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

    (pendingTransactionsManager ? PendingTransactionsManager.GetPendingTransactions).mapTo[PendingTransactionsResponse]
      .recover { case ex =>
        log.error(ex, "Failed to get transactions, mining block with empty transactions list")
        PendingTransactionsResponse(Nil)
      }
  }
}

object Miner {
  def props(blockchain: Blockchain,
    blockGenerator: BlockGenerator,
    ommersPool: ActorRef,
    pendingTransactionsManager: ActorRef,
    syncController: ActorRef,
    miningConfig: MiningConfig,
    ethService: EthService,
    consensus: Consensus): Props =
    Props(new Miner(blockchain, blockGenerator, ommersPool,
      pendingTransactionsManager, syncController, miningConfig, ethService, consensus))

  case object StartMining
  case object StopMining

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

