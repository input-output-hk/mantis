package io.iohk.ethereum.ledger

import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.vm.Storage

/** Storage content of an account (ex: stores smart-contract values)
  *
  * @param wrapped a key-value (kec key -> rlp value) based on an MPT
  */
class InMemoryWorldStateProxyStorage(
    val wrapped: InMemorySimpleMapProxy[BigInt, BigInt, MerklePatriciaTrie[BigInt, BigInt]]
) extends Storage[InMemoryWorldStateProxyStorage] {

  override def store(addr: BigInt, value: BigInt): InMemoryWorldStateProxyStorage = {
    val newWrapped =
      if (value == 0) wrapped - addr
      else wrapped + (addr -> value)
    new InMemoryWorldStateProxyStorage(newWrapped)
  }

  override def load(addr: BigInt): BigInt = wrapped.get(addr).getOrElse(0)
}
