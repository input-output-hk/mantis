package io.iohk.ethereum.transactionTest

import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.transactionTest.util.FixtureProvider
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

class ContractTest extends FlatSpec with Matchers {
  val noErrors: Right[Nothing, Unit] = Right(())

  "Ledger" should "transfer ether" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/transactionTest/purchaseContruct/")

    val (storage, stateStorage) = FixtureProvider.prepareStorages(0, fixtures)

    //block only with ether transfers
    Ledger.executeBlock(fixtures.blockByNumber(1), storage, stateStorage) shouldBe noErrors
  }

  ignore should "deploy contract" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/transactionTest/purchaseContruct/")

    val (storage, stateStorage) = FixtureProvider.prepareStorages(1, fixtures)

    //contract creation
    Ledger.executeBlock(fixtures.blockByNumber(2), storage, stateStorage) shouldBe noErrors
  }

  it should "execute contract call" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/transactionTest/purchaseContruct/")

    val (storage, stateStorage) = FixtureProvider.prepareStorages(2, fixtures)

    //block with ether transfers and contract call
    Ledger.executeBlock(fixtures.blockByNumber(3), storage, stateStorage) shouldBe noErrors
  }

  it should "execute contract that pays 2 accounts" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/transactionTest/purchaseContruct/")

    val (storage, stateStorage) = FixtureProvider.prepareStorages(2, fixtures)

    //block contains contract paying 2 accounts
    Ledger.executeBlock(fixtures.blockByNumber(3), storage, stateStorage) shouldBe noErrors
  }
}
