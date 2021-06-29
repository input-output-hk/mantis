package io.iohk.ethereum

import io.iohk.ethereum.vm.VM
import io.iohk.ethereum.vm.ProgramContext
import io.iohk.ethereum.vm.ProgramResult

package object ledger {
  type VMImpl = VM[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PC = ProgramContext[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PR = ProgramResult[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
}
