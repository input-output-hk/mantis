package io.iohk.ethereum.mining

import java.io.{File, FileInputStream, FileOutputStream}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.sync.RegularSync
import io.iohk.ethereum.consensus.Ethash
import io.iohk.ethereum.consensus.Ethash.ProofOfWork
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.MiningConfig
import org.spongycastle.util.encoders.Hex

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

class Miner(
    appStateStorage: AppStateStorage,
    blockGenerator: BlockGenerator,
    ommersPool: ActorRef,
    pendingTransactionsManager: ActorRef,
    syncController: ActorRef,
    miningConfig: MiningConfig)
  extends Actor with ActorLogging {

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
    val blockNumber = appStateStorage.getBestBlockNumber() + 1
    val epoch = Ethash.epoch(blockNumber.toLong)

    val (dag, dagSize) = (currentEpoch, currentEpochDag, currentEpochDagSize) match {
      case (Some(`epoch`), Some(dag), Some(dagSize)) => (dag, dagSize)
      case _ =>
        val seed = Ethash.seed(epoch)
        val dagSize = Ethash.dagSize(epoch)
        val dag =
          if (!dagFile(seed).exists()) generateDagAndSaveToFile(epoch, dagSize, seed)
          else loadDagFromFile(seed, dagSize)

        currentEpoch = Some(epoch)
        currentEpochDag = Some(dag)
        currentEpochDagSize = Some(dagSize)
      (dag, dagSize)
    }

    getBlockForMining(blockNumber) onComplete {
      case Success(PendingBlock(block, _)) =>
        val headerHash = crypto.kec256(BlockHeader.getEncodedWithoutNonce(block.header))
        val mineResult = mine(headerHash, block.header.difficulty.toLong, dagSize, dag, miningConfig.mineRounds)
        mineResult.foreach { case (pow, nonce) =>
          syncController ! RegularSync.MinedBlock(block.copy(header = block.header.copy(nonce = nonce, mixHash = pow.mixHash)))
        }
        self ! ProcessMining

      case Failure(ex) =>
        log.error("Unable to get block for mining", ex)
        context.system.scheduler.scheduleOnce(10.seconds, self, ProcessMining)
    }
  }

  private def dagFile(seed: ByteString): File = {
    new File(s"${miningConfig.ethashDir}/full-R${Ethash.Revision}-${Hex.toHexString(seed.take(8).toArray[Byte])}")
  }

  private def generateDagAndSaveToFile(epoch: Long, dagSize: Long, seed: ByteString): Array[Array[Int]] = {
    // scalastyle:off magic.number

    val file = dagFile(seed)
    if (file.exists()) file.delete()
    file.createNewFile()

    val outputStream = new FileOutputStream(dagFile(seed).getAbsolutePath)
    outputStream.write(Array(0xfe, 0xca, 0xdd, 0xba, 0xad, 0xde, 0xe1, 0xfe).map(_.toByte))

    val cache = Ethash.makeCache(epoch)
    val res = new Array[Array[Int]]((dagSize / Ethash.HASH_BYTES).toInt)

    val max = (dagSize / Ethash.HASH_BYTES).toInt
    (0 until max).foreach { i =>
      val item = Ethash.calcDatasetItem(cache, i)
      outputStream.write(Ethash.intsToBytes(item))
      res(i) = item

      if (i % 100000 == 0) log.info(s"Generating DAG ${((i / max.toDouble) * 100).toInt}%")
    }

    Try(outputStream.close())

    res
  }

  private def loadDagFromFile(seed: ByteString, dagSize: Long): Array[Array[Int]] = {
    val inputStream = new FileInputStream(dagFile(seed).getAbsolutePath)
    inputStream.skip(8)
    val buffer = new Array[Byte](64)
    val res = new Array[Array[Int]]((dagSize / Ethash.HASH_BYTES).toInt)
    var index = 0

    while (inputStream.read(buffer) > 0) {
      if (index % 100000 == 0) log.info(s"Loading DAG from file ${((index / res.length.toDouble) * 100).toInt}%")
      res(index) = Ethash.bytesToInts(buffer)
      index += 1
    }

    Try(inputStream.close())

    res
  }

  private def mine(headerHash: Array[Byte], difficulty: Long, dagSize: Long, dag: Array[Array[Int]], numRounds: Int): Option[(ProofOfWork, ByteString)] = {
    // scalastyle:off magic.number
    val initNonce = BigInt(64, new Random())

    (0 to numRounds).toStream.map { n =>
      val nonce = (initNonce + n) % MaxNonce
      val pow = Ethash.hashimoto(headerHash, nonce.toByteArray, dagSize, dag.apply)
      (Ethash.checkDifficulty(difficulty, pow), pow, nonce)
    }.collectFirst { case (true, pow, nonce) => (pow, ByteString(nonce.toByteArray)) }
  }

  private def getBlockForMining(blockNumber: BigInt): Future[PendingBlock] = {
    getOmmersFromPool(blockNumber).zip(getTransactionsFromPool).flatMap { case (ommers, pendingTxs) =>
      blockGenerator.generateBlockForMining(blockNumber, pendingTxs.pendingTransactions.map(_.stx), ommers.headers, miningConfig.coinbase) match {
        case Right(pb) => Future.successful(pb)
        case Left(err) => Future.failed(new RuntimeException(s"Error while generating block for mining: $err"))
      }
    }
  }

  private def getOmmersFromPool(blockNumber: BigInt) = {
    implicit val timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

    (ommersPool ? OmmersPool.GetOmmers(blockNumber)).mapTo[OmmersPool.Ommers]
      .recover { case ex =>
        log.error("Failed to get ommers, mining block with empty ommers list", ex)
        OmmersPool.Ommers(Nil)
      }
  }

  private def getTransactionsFromPool = {
    implicit val timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

    (pendingTransactionsManager ? PendingTransactionsManager.GetPendingTransactions).mapTo[PendingTransactionsResponse]
      .recover { case ex =>
        log.error("Failed to get transactions, mining block with empty transactions list", ex)
        PendingTransactionsResponse(Nil)
      }
  }
}

object Miner {
  def props(appStateStorage: AppStateStorage,
            blockGenerator: BlockGenerator,
            ommersPool: ActorRef,
            pendingTransactionsManager: ActorRef,
            syncController: ActorRef,
            miningConfig: MiningConfig): Props =
    Props(new Miner(appStateStorage, blockGenerator, ommersPool, pendingTransactionsManager, syncController, miningConfig))

  case object StartMining
  case object StopMining

  private case object ProcessMining

  // scalastyle:off magic.number
  val MaxNonce: BigInt = BigInt(2).pow(64)
}
