package io.iohk.ethereum.transactionTest

import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.transactionTest.util.FixtureProvider
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

class ForksTest extends FlatSpec with Matchers {
  val noErrors: Right[Nothing, Unit] = Right(())

  "Ledger" should "execute blocks with respect to forks" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/transactionTest/forksTest/")

    val startBlock = 1
    val lastBlock = 11

    (startBlock to lastBlock) foreach { blockNumber =>
      val (storage, stateStorage) = FixtureProvider.prepareStorages(blockNumber - 1, fixtures)
      println("\n\n\n\nblock: " + blockNumber)
      Ledger.executeBlock(fixtures.blockByNumber(blockNumber), storage, stateStorage) shouldBe noErrors
    }
  }

}
