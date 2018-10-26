package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.ledger.{ InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage }

trait FakeBlockchain extends Blockchain {
  override type S = InMemoryWorldStateProxyStorage
  override type WS = InMemoryWorldStateProxy
}
