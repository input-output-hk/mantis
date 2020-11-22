package io.iohk.ethereum.domain

import io.iohk.ethereum.Fixtures.Blocks.Block3125369
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class BlockBodyTest extends AnyWordSpec with Matchers {

  "BlockBodyTest" should {

    "construct a reverseIterator" in {
      val nonces = Block3125369.body.toIndexedSeq.map(_.tx.nonce)
      val reverseHashes = Block3125369.body.reverseIterator.map(_.tx.nonce).toStream
      nonces.reverse shouldEqual reverseHashes
    }

    "enumerate the same way as zipWithIndex" in {
      val zipped = Block3125369.body.toIndexedSeq.zipWithIndex
      val enumerated = Block3125369.body.enumerate.toStream
      zipped shouldEqual enumerated
    }

    "withTransactions should work the same way as copy" in {
      val copy = Block3125369.body.withTransactions(Nil)
      copy.numberOfTxs shouldEqual 0
      copy.toIndexedSeq.size shouldEqual 0
      copy.numberOfUncles shouldEqual Block3125369.body.numberOfUncles

      val secondCopy = copy.withTransactions(Block3125369.body.toSeq)
      Block3125369.body shouldEqual secondCopy
    }

    "findWhere should find first matched result" in {
      val headFound = Block3125369.body.findWhere(_ => true).map {  case (i, tx) => i -> tx.tx.nonce }
      headFound shouldEqual Some( 0 -> Block3125369.body.head.tx.nonce )
    }

    "findWhere should return None when predicate is always false" in {
      val headFound = Block3125369.body.findWhere(_ => false).map {  case (i, tx) => i -> tx.tx.nonce }
      headFound shouldEqual None
    }

    "getTransactionByIndex should return None on any index out of bounds" in {
      val txnCount = Block3125369.body.numberOfTxs
      val validRange = (0 until txnCount)
      assert(txnCount > 1)

      val seq = Block3125369.body.toIndexedSeq

      for (i <- 0 until 1000) {
        // wider range of possible indices, including negative
        val randomIndex = Random.nextInt(txnCount * 3) - txnCount
        val foundTxn = Block3125369.body.getTransactionByIndex(randomIndex)
        foundTxn.nonEmpty shouldEqual validRange.contains(randomIndex)
        if (foundTxn.isDefined) {
          foundTxn shouldEqual Some(seq(randomIndex))
        }
      }
    }

    "findWhere should return transaction and its correct index" in {
      val txns = Block3125369.body
      for (i <- 0 until 1000) {
        val correctIndex = Random.nextInt(txns.size)
        val correctTxn = txns.getTransactionByIndex(correctIndex).get
        val guess = txns.findWhere(_.tx.nonce == correctTxn.tx.nonce)
        guess shouldEqual Some(correctIndex -> correctTxn)
      }
    }


  }
}
