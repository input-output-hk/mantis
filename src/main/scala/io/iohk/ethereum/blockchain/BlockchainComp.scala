package io.iohk.ethereum.blockchain

import io.iohk.ethereum.db.components.StoragesComp

trait BlockchainComp {
  val storagesComp: StoragesComp

  val blockchain: Blockchain
}
