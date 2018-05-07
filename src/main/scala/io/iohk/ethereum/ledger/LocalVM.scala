package io.iohk.ethereum.ledger

import io.iohk.ethereum.vm.VM

object LocalVM extends VM[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
