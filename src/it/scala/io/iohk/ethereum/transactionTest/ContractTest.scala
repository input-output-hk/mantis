package io.iohk.ethereum.transactionTest

import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.transactionTest.util.FixtureProvider
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

class ContractTest extends FlatSpec with Matchers {
  "Ledger" should "transfer ether" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/transactionTest/purchaseContruct/")

    val (storage, stateStorage) = FixtureProvider.prepareStorages(0, fixtures)

    //block only with ether transfers
    Ledger.executeBlock(fixtures.blockByNumber(1), storage, stateStorage)

    //block 2 contains contract creation
    //block 3 and 4 contains contract calls with contract storage modification
    //block 4 contains contract paying to 2 accounts
  }

  it should "deploy contract" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/transactionTest/purchaseContruct/")

    val (storage, stateStorage) = FixtureProvider.prepareStorages(1, fixtures)

    //contract creation
    Ledger.executeBlock(fixtures.blockByNumber(2), storage, stateStorage)
  }

  ignore should "execute contract call" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/transactionTest/purchaseContruct/")

    val (storage, stateStorage) = FixtureProvider.prepareStorages(2, fixtures)

    //block with ether transfers and contract call
    Ledger.executeBlock(fixtures.blockByNumber(3), storage, stateStorage)
  }
}
