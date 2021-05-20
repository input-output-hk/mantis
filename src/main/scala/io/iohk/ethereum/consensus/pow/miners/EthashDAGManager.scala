package io.iohk.ethereum.consensus.pow.miners

import akka.util.ByteString
import io.iohk.ethereum.consensus.pow.miners.EthashMiner.DagFilePrefix
import io.iohk.ethereum.consensus.pow.{EthashUtils, PoWBlockCreator}
import io.iohk.ethereum.utils.{ByteUtils, Logger}
import org.bouncycastle.util.encoders.Hex

import java.io.{File, FileInputStream, FileOutputStream}
import scala.util.{Failure, Success, Try}

class EthashDAGManager(blockCreator: PoWBlockCreator) extends Logger {
  var currentEpoch: Option[Long] = None
  var currentEpochDagSize: Option[Long] = None
  var currentEpochDag: Option[Array[Array[Int]]] = None

  def calculateDagSize(blockNumber: Long, epoch: Long): (Array[Array[Int]], Long) = {
    (currentEpoch, currentEpochDag, currentEpochDagSize) match {
      case (_, Some(dag), Some(dagSize)) => (dag, dagSize)
      case _ =>
        val seed = EthashUtils.seed(blockNumber, blockCreator.blockchainConfig.ecip1099BlockNumber.toLong)
        val dagSize = EthashUtils.dagSize(epoch)
        val dagNumHashes = (dagSize / EthashUtils.HASH_BYTES).toInt
        val dag =
          if (!dagFile(seed).exists()) generateDagAndSaveToFile(epoch, dagNumHashes, seed)
          else {
            val res = loadDagFromFile(seed, dagNumHashes)
            res.failed.foreach { ex =>
              log.error("Cannot read DAG from file", ex)
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
      s"${blockCreator.miningConfig.ethashDir}/full-R${EthashUtils.Revision}-${Hex
        .toHexString(seed.take(8).toArray[Byte])}"
    )
  }

  private def generateDagAndSaveToFile(epoch: Long, dagNumHashes: Int, seed: ByteString): Array[Array[Int]] = {
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
      val buffer = new Array[Byte](64) // scalastyle:ignore magic.number
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
}
