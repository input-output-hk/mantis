package io.iohk.ethereum.snappy

import io.iohk.ethereum.domain.{Block, Blockchain, Receipt}
import io.iohk.ethereum.utils.Logger
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._

class SnappyTest extends FreeSpec with Matchers with Logger {

  val config = Config()
  val pre = new Prerequisites(config)
  import pre._

  "Blockchain regression test" in {

    val startN = targetBlockchain match {
      case Some(tb) => config.startBlock.getOrElse(findHighestBlockNumber(tb) - 1).max(1)
      case None => BigInt(1)
    }
    val targetN = config.targetBlock.getOrElse(findHighestBlockNumber(sourceBlockchain)).max(1)

    val progLog = new ProgressLogger(startN, targetN, 5.seconds)

    progLog.start()

    for (n <- startN to targetN) {
      val block: Block = sourceBlockchain.getBlockByNumber(n)
        .getOrElse(fail(s"Failed to retrieve block by number: $n"))

      val expectedReceipts = sourceBlockchain.getReceiptsByHash(block.header.hash)
        .getOrElse(fail(s"Failed to retrieve receipts for block number: $n"))

      val result = executeBlock(block)

      result match {
        case Left(error) =>
          fail(s"Failed to execute block $n: $error")

        case Right(receipts) =>
          if (receipts == expectedReceipts) {
            targetBlockchain.foreach(_.save(block))
          } else {
            fail(s"Block $n did not execute correctly.\n$receipts did not equal $expectedReceipts")
          }
      }

      progLog.update(n)
    }
  }

  private def executeBlock(block: Block): Either[Any, Seq[Receipt]] =
    targetStorages match {
      case Some(storages) =>
        ledger.executeBlock(block, storages.storages, validators)

      case None =>
        // this seems to discard failures, for better errors messages we might want to implement a different method (simulateBlock?)
        val result = ledger.prepareBlock(block, sourceStorages.storages, validators)
        Right(result.blockResult.receipts)
    }

  private def findHighestBlockNumber(blockchain: Blockchain, n: BigInt = 1000000, bottom: BigInt = 0, top: BigInt = -1): BigInt = {
    if (top - bottom == 1)
      n

    else if (top < 0) {
      def candidates(n: BigInt): Stream[BigInt] = n #:: candidates(n + 1000000)
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
