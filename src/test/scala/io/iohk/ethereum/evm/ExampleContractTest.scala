package io.iohk.ethereum.evm

import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.domain.{Block, BlockchainStorages}
import io.iohk.ethereum.evm.util.FixtureProvider
import io.iohk.ethereum.ledger.Ledger
import org.scalatest.{FlatSpec, Matchers}

class ExampleContractTest extends FlatSpec with Matchers {
  "FixtureProvider" should " load data from files" in {
    //val v = FixtureProvider.loadFixtures("/io/iohk/ethereum/evm/purchaseContruct/")
    //v.toString
    //todo check data
//    val storages: BlockchainStorages =
//    val stateStorage: NodeStorage =
//
//    Ledger.executeBlock(v.blockByNumber(3), storages, stateStorage)
  }
}
