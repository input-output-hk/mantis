package io.iohk.ethereum.snappy

import java.io.File

import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.utils.Logger
import org.apache.commons.io.FileUtils
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._

class SnappyTest extends FreeSpec with Matchers with Logger {

  "Blockchain regression test" in {

    val config = new Config()
    removeTargetDir(config.targetDbPath)

    val pre = new Prerequisites(config)
    import pre._

    val targetN = config.targetBlock.getOrElse(findHighestBlockNumber(sourceBlockchain))
    val progLog = new ProgressLogger(targetN, 2.seconds)

    progLog.start()

    for (n <- BigInt(1) to targetN) {
      val block: Block = sourceBlockchain.getBlockByNumber(n)
        .getOrElse(fail(s"Failed to retrieve block by number: $n"))

      val expectedReceipts = sourceBlockchain.getReceiptsByHash(block.header.hash)
        .getOrElse(fail(s"Failed to retrieve receipts for block number: $n"))

      val result = ledger.executeBlock(block, targetStorages.storages, validators)

      result match {
        case Left(error) =>
          fail(s"Failed to execute block $n: $error")

        case Right(receipts) =>
          receipts shouldEqual expectedReceipts
      }

      targetBlockchain.save(block)

      progLog.update(n)
    }
  }

  private def findHighestBlockNumber(blockchain: Blockchain, n: BigInt = 1000000, lastN: BigInt = 0): BigInt =
    if (n <= 0)
      fail("No block found in the source DB!")
    else if (n == lastN)
      n
    else {
      val newN = blockchain.getBlockByNumber(n) match {
        case Some(_) => if (n > lastN) n + n - lastN else (n + lastN) / 2
        case None => (n + lastN) / 2
      }
      findHighestBlockNumber(blockchain, newN, n)
    }

  private def removeTargetDir(dirPath: String): Unit = {
    val dir = new File(dirPath)
    if (dir.exists() && dir.isDirectory) {
      FileUtils.deleteDirectory(dir)
    }
  }

}
