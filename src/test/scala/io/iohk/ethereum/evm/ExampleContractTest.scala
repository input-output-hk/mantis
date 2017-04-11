package io.iohk.ethereum.evm

import io.iohk.ethereum.evm.util.FixtureProvider
import io.iohk.ethereum.ledger.Ledger
import org.scalatest.{FlatSpec, Matchers}

class ExampleContractTest extends FlatSpec with Matchers {
  "FixtureProvider" should " load data from files" in {
    //val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/evm_test/purchaseContruct/")

    //val (storage, stateStorage) = FixtureProvider.prepareStorages(0,fixtures)

    //block containing only ether transfers
    //Ledger.executeBlock(fixtures.blockByNumber(1), storage, stateStorage)
  }
}
