package io.iohk.ethereum.snappy

import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.utils.Logger
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._

class SnappyTest extends FreeSpec with Matchers with Logger {

  "Blockchain regression test" in {

    val config = new Config()
    val pre = new Prerequisites(config)
    import pre._

    val startN = config.startBlock.getOrElse(findHighestBlockNumber(targetBlockchain) - 1).max(1)
    val targetN = config.targetBlock.getOrElse(findHighestBlockNumber(sourceBlockchain)).max(1)
    val progLog = new ProgressLogger(startN, targetN, 2.seconds)

    progLog.start()

    for (n <- startN to targetN) {
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

  private def findHighestBlockNumber(blockchain: Blockchain, n: BigInt = 1000000, bottom: BigInt = 0, top: BigInt = -1): BigInt = {
    if (top - bottom == 1)
      n

    else if (top < 0) {
      def candidates(n: BigInt): Stream[BigInt] = n #:: candidates(n + 100000)
      val newTop = candidates(1).find(n => blockchain.getBlockByNumber(n).isEmpty).get
      findHighestBlockNumber(blockchain, newTop / 2, 0, newTop)
    }

    else {
      val (newBottom, newTop) = blockchain.getBlockByNumber(n) match {
        case Some(_) => (n, top)
        case None => (bottom, n)
      }
      findHighestBlockNumber(blockchain, (bottom + top) / 2, newBottom, newTop)
    }
  }

}
