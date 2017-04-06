package io.iohk.ethereum.evm

import org.scalatest.{FlatSpec, Matchers}

class ExampleContractTest extends FlatSpec with Matchers {
  "FixtureProvider" should " load data from files" in {
    val v = FixtureProvider.loadFixtures("/io/iohk/ethereum/evm/purchaseContruct/")
    //todo check data
    println(v)
  }
}
