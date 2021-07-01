package io.iohk.ethereum.testmode

import akka.util.ByteString

import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.db.storage.EvmCodeStorage.Code
import io.iohk.ethereum.db.storage.MptStorage
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Account.accountSerializer
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.ledger.InMemorySimpleMapProxy
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.InMemoryWorldStateProxyStorage
import io.iohk.ethereum.mpt.MerklePatriciaTrie

/** This is a wrapper around InMemoryWorldStateProxy.
  * Its only role is to store the storage key encountered during a run to store them for debugging purpose.
  */
case class TestModeWorldStateProxy(
    override val stateStorage: MptStorage,
    override val accountsStateTrie: InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]],
    // format: off
    override val contractStorages: Map[Address, InMemorySimpleMapProxy[BigInt, BigInt, MerklePatriciaTrie[BigInt, BigInt]]],
    // format: on
    override val evmCodeStorage: EvmCodeStorage,
    override val accountCodes: Map[Address, Code],
    override val getBlockByNumber: (BigInt) => Option[ByteString],
    override val accountStartNonce: UInt256,
    override val touchedAccounts: Set[Address],
    override val noEmptyAccountsCond: Boolean,
    override val ethCompatibleStorage: Boolean,
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
    copy(accountsStateTrie = accountsStateTrie.put(address, account))

  override def deleteAccount(address: Address): TestModeWorldStateProxy =
    copy(
      accountsStateTrie = accountsStateTrie.remove(address),
      contractStorages = contractStorages - address,
      accountCodes = accountCodes - address
    )

  override def touchAccounts(addresses: Address*): TestModeWorldStateProxy =
    if (noEmptyAccounts)
      copy(touchedAccounts = touchedAccounts ++ addresses.toSet)
    else
      this

  override def clearTouchedAccounts: TestModeWorldStateProxy =
    copy(touchedAccounts = touchedAccounts.empty)

  override def keepPrecompileTouched(world: InMemoryWorldStateProxy): TestModeWorldStateProxy =
    if (world.touchedAccounts.contains(ripmdContractAddress))
      copy(touchedAccounts = touchedAccounts + ripmdContractAddress)
    else
      this

  override def saveCode(address: Address, code: ByteString): TestModeWorldStateProxy =
    copy(accountCodes = accountCodes + (address -> code))

  override def saveStorage(address: Address, storage: InMemoryWorldStateProxyStorage): TestModeWorldStateProxy = {
    storage.wrapped.cache.foreach { case (key, _) => saveStoragePreimage(UInt256(key)) }
    copy(contractStorages = contractStorages + (address -> storage.wrapped))
  }
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
  ): TestModeWorldStateProxy =
    new TestModeWorldStateProxy(
      stateStorage = nodesKeyValueStorage,
      accountsStateTrie = createProxiedAccountsStateTrie(nodesKeyValueStorage, stateRootHash),
      contractStorages = Map.empty,
      evmCodeStorage = evmCodeStorage,
      accountCodes = Map.empty,
      getBlockByNumber = getBlockHashByNumber,
      accountStartNonce = accountStartNonce,
      touchedAccounts = Set.empty,
      noEmptyAccountsCond = noEmptyAccounts,
      ethCompatibleStorage = ethCompatibleStorage,
      saveStoragePreimage = saveStoragePreimage
    )

  private def createProxiedAccountsStateTrie(
      accountsStorage: MptStorage,
      stateRootHash: ByteString
  ): InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]] =
    InMemorySimpleMapProxy.wrap[Address, Account, MerklePatriciaTrie[Address, Account]](
      MerklePatriciaTrie[Address, Account](
        stateRootHash.toArray[Byte],
        accountsStorage
      )(Address.hashedAddressEncoder, accountSerializer)
    )
}
