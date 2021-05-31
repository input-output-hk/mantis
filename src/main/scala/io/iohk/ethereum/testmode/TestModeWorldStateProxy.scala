package io.iohk.ethereum.testmode

import akka.util.ByteString
import io.iohk.ethereum.db.storage.{EvmCodeStorage, MptStorage}
import io.iohk.ethereum.db.storage.EvmCodeStorage.Code
import io.iohk.ethereum.domain.Account.accountSerializer
import io.iohk.ethereum.domain.{Account, Address, UInt256}
import io.iohk.ethereum.ledger.{InMemorySimpleMapProxy, InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.mpt.MerklePatriciaTrie

/** This is a wrapper around InMemoryWorldStateProxy.
  * Its only role is to store the storage key encountered during a run to store them for debugging purpose.
  */
class TestModeWorldStateProxy(
    stateStorage: MptStorage,
    accountsStateTrie: InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]],
    contractStorages: Map[Address, InMemorySimpleMapProxy[BigInt, BigInt, MerklePatriciaTrie[BigInt, BigInt]]],
    evmCodeStorage: EvmCodeStorage,
    accountCodes: Map[Address, Code],
    getBlockByNumber: (BigInt) => Option[ByteString],
    accountStartNonce: UInt256,
    touchedAccounts: Set[Address],
    noEmptyAccountsCond: Boolean,
    ethCompatibleStorage: Boolean,
    saveStoragePreimage: (UInt256) => Unit
) extends InMemoryWorldStateProxy(
      stateStorage,
      accountsStateTrie,
      contractStorages,
      evmCodeStorage,
      accountCodes,
      getBlockByNumber,
      accountStartNonce,
      touchedAccounts,
      noEmptyAccountsCond,
      ethCompatibleStorage
    ) {

  override def saveAccount(address: Address, account: Account): TestModeWorldStateProxy =
    copyWith(accountsStateTrie = accountsStateTrie.put(address, account))

  override def deleteAccount(address: Address): TestModeWorldStateProxy =
    copyWith(
      accountsStateTrie = accountsStateTrie.remove(address),
      contractStorages = contractStorages - address,
      accountCodes = accountCodes - address
    )

  override def touchAccounts(addresses: Address*): TestModeWorldStateProxy =
    if (noEmptyAccounts)
      copyWith(touchedAccounts = touchedAccounts ++ addresses.toSet)
    else
      this

  override def clearTouchedAccounts: TestModeWorldStateProxy =
    copyWith(touchedAccounts = touchedAccounts.empty)

  override def keepPrecompileTouched(world: InMemoryWorldStateProxy): TestModeWorldStateProxy = {
    if (world.touchedAccounts.contains(ripmdContractAddress))
      copyWith(touchedAccounts = touchedAccounts + ripmdContractAddress)
    else
      this
  }

  override def saveCode(address: Address, code: ByteString): TestModeWorldStateProxy =
    copyWith(accountCodes = accountCodes + (address -> code))

  override def saveStorage(address: Address, storage: InMemoryWorldStateProxyStorage): TestModeWorldStateProxy = {
    storage.wrapped.cache.foreach { case (key, _) => saveStoragePreimage(UInt256(key)) }
    copyWith(contractStorages = contractStorages + (address -> storage.wrapped))
  }

  private def copyWith(
      stateStorage: MptStorage = stateStorage,
      accountsStateTrie: InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]] =
        accountsStateTrie,
      contractStorages: Map[Address, InMemorySimpleMapProxy[BigInt, BigInt, MerklePatriciaTrie[BigInt, BigInt]]] =
        contractStorages,
      evmCodeStorage: EvmCodeStorage = evmCodeStorage,
      accountCodes: Map[Address, Code] = accountCodes,
      touchedAccounts: Set[Address] = touchedAccounts
  ): TestModeWorldStateProxy =
    new TestModeWorldStateProxy(
      stateStorage,
      accountsStateTrie,
      contractStorages,
      evmCodeStorage,
      accountCodes,
      getBlockByNumber,
      accountStartNonce,
      touchedAccounts,
      noEmptyAccountsCond,
      ethCompatibleStorage,
      saveStoragePreimage
    )
}

object TestModeWorldStateProxy {
  def apply(
      evmCodeStorage: EvmCodeStorage,
      nodesKeyValueStorage: MptStorage,
      accountStartNonce: UInt256,
      getBlockHashByNumber: BigInt => Option[ByteString],
      stateRootHash: ByteString,
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean,
      saveStoragePreimage: (UInt256) => Unit
  ): TestModeWorldStateProxy = {
    val accountsStateTrieProxy = createProxiedAccountsStateTrie(nodesKeyValueStorage, stateRootHash)
    new TestModeWorldStateProxy(
      nodesKeyValueStorage,
      accountsStateTrieProxy,
      Map.empty,
      evmCodeStorage,
      Map.empty,
      getBlockHashByNumber,
      accountStartNonce,
      Set.empty,
      noEmptyAccounts,
      ethCompatibleStorage,
      saveStoragePreimage
    )
  }

  private def createProxiedAccountsStateTrie(
      accountsStorage: MptStorage,
      stateRootHash: ByteString
  ): InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]] = {
    InMemorySimpleMapProxy.wrap[Address, Account, MerklePatriciaTrie[Address, Account]](
      MerklePatriciaTrie[Address, Account](
        stateRootHash.toArray[Byte],
        accountsStorage
      )(Address.hashedAddressEncoder, accountSerializer)
    )
  }
}
