package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.EvmCodeStorage.Code
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.vm.{Storage, WorldStateProxy}

object InMemoryWorldStateProxy {

  import Account._

  def apply(
      evmCodeStorage: EvmCodeStorage,
      nodesKeyValueStorage: MptStorage,
      accountStartNonce: UInt256,
      getBlockHashByNumber: BigInt => Option[ByteString],
      stateRootHash: ByteString,
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): InMemoryWorldStateProxy = {
    val accountsStateTrieProxy = createProxiedAccountsStateTrie(nodesKeyValueStorage, stateRootHash)
    new InMemoryWorldStateProxy(
      nodesKeyValueStorage,
      accountsStateTrieProxy,
      Map.empty,
      evmCodeStorage,
      Map.empty,
      getBlockHashByNumber,
      accountStartNonce,
      Set.empty,
      noEmptyAccounts,
      ethCompatibleStorage
    )
  }

  /**
    * Updates state trie with current changes but does not persist them into the storages. To do so it:
    *   - Commits code (to get account's code hashes)
    *   - Commits constract storages (to get account's contract storage root)
    *   - Updates state tree
    *
    * @param worldState Proxy to commit
    * @return Updated world
    */
  def persistState(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    def persistCode(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
      worldState.accountCodes.foldLeft(worldState) { case (updatedWorldState, (address, code)) =>
        val codeHash = kec256(code)
        updatedWorldState.evmCodeStorage.put(codeHash, code).commit()
        updatedWorldState.copyWith(
          accountsStateTrie = updatedWorldState.accountsStateTrie +
            (address -> updatedWorldState.getGuaranteedAccount(address).copy(codeHash = codeHash)),
          accountCodes = Map.empty
        )
      }
    }

    def persistContractStorage(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
      worldState.contractStorages.foldLeft(worldState) { case (updatedWorldState, (address, storageTrie)) =>
        val persistedStorage = storageTrie.persist()
        val newStorageRootHash = persistedStorage.inner.getRootHash

        updatedWorldState.copyWith(
          contractStorages = updatedWorldState.contractStorages + (address -> persistedStorage),
          accountsStateTrie = updatedWorldState.accountsStateTrie +
            (address -> updatedWorldState
              .getGuaranteedAccount(address)
              .copy(storageRoot = ByteString(newStorageRootHash)))
        )
      }

    def persistAccountsStateTrie(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
      worldState.copyWith(accountsStateTrie = worldState.accountsStateTrie.persist())

    (persistCode _ andThen persistContractStorage andThen persistAccountsStateTrie)(worldState)
  }

  /**
    * Returns an [[InMemorySimpleMapProxy]] of the accounts state trie "The world state (state), is a mapping
    * between Keccak 256-bit hashes of the addresses (160-bit identifiers) and account states (a data structure serialised as RLP [...]).
    * Though not stored on the blockchain, it is assumed that the implementation will maintain this mapping in a
    * modified Merkle Patricia tree [...])."
    *
    * * See [[http://paper.gavwood.com YP 4.1]]
    *
    * @param accountsStorage Accounts Storage where trie nodes are saved
    * @param stateRootHash   State trie root hash
    * @return Proxied Accounts State Trie
    */
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

class InMemoryWorldStateProxy private[ledger] (
    // State MPT proxied nodes storage needed to construct the storage MPT when calling [[getStorage]].
    // Accounts state and accounts storage states are saved within the same storage
    val stateStorage: MptStorage,
    val accountsStateTrie: InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]],
    // Contract Storage Proxies by Address
    val contractStorages: Map[Address, InMemorySimpleMapProxy[BigInt, BigInt, MerklePatriciaTrie[BigInt, BigInt]]],
    //It's easier to use the storage instead of the blockchain here (because of proxy wrapping). We might need to reconsider this
    val evmCodeStorage: EvmCodeStorage,
    // Account's code by Address
    val accountCodes: Map[Address, Code],
    val getBlockByNumber: (BigInt) => Option[ByteString],
    override val accountStartNonce: UInt256,
    // touchedAccounts and noEmptyAccountsCond are introduced by EIP161 to track accounts touched during the transaction
    // execution. Touched account are only added to Set if noEmptyAccountsCond == true, otherwise all other operations
    // operate on empty set.
    val touchedAccounts: Set[Address],
    val noEmptyAccountsCond: Boolean,
    val ethCompatibleStorage: Boolean
) extends WorldStateProxy[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage] {

  override def getAccount(address: Address): Option[Account] = accountsStateTrie.get(address)

  override def getEmptyAccount: Account = Account.empty(accountStartNonce)

  override def getGuaranteedAccount(address: Address): Account = super.getGuaranteedAccount(address)

  override def saveAccount(address: Address, account: Account): InMemoryWorldStateProxy = {
    copyWith(accountsStateTrie = accountsStateTrie.put(address, account))
  }

  override def deleteAccount(address: Address): InMemoryWorldStateProxy =
    copyWith(
      accountsStateTrie = accountsStateTrie.remove(address),
      contractStorages = contractStorages - address,
      accountCodes = accountCodes - address
    )

  override def getCode(address: Address): ByteString =
    accountCodes.getOrElse(
      address,
      getAccount(address).flatMap(account => evmCodeStorage.get(account.codeHash)).getOrElse(ByteString.empty)
    )

  override def getStorage(address: Address): InMemoryWorldStateProxyStorage =
    new InMemoryWorldStateProxyStorage(contractStorages.getOrElse(address, getStorageForAddress(address, stateStorage)))

  override def saveCode(address: Address, code: ByteString): InMemoryWorldStateProxy =
    copyWith(accountCodes = accountCodes + (address -> code))

  override def saveStorage(address: Address, storage: InMemoryWorldStateProxyStorage): InMemoryWorldStateProxy =
    copyWith(contractStorages = contractStorages + (address -> storage.wrapped))

  override def touchAccounts(addresses: Address*): InMemoryWorldStateProxy =
    if (noEmptyAccounts)
      copyWith(touchedAccounts = touchedAccounts ++ addresses.toSet)
    else
      this

  override def clearTouchedAccounts: InMemoryWorldStateProxy =
    copyWith(touchedAccounts = touchedAccounts.empty)

  override def noEmptyAccounts: Boolean = noEmptyAccountsCond

  override def keepPrecompileTouched(world: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    if (world.touchedAccounts.contains(ripmdContractAddress))
      copyWith(touchedAccounts = touchedAccounts + ripmdContractAddress)
    else
      this
  }

  /**
    * Returns world state root hash. This value is only updated after persist.
    */
  def stateRootHash: ByteString = ByteString(accountsStateTrie.inner.getRootHash)

  private def getStorageForAddress(address: Address, stateStorage: MptStorage) = {
    val storageRoot = getAccount(address)
      .map(account => account.storageRoot)
      .getOrElse(Account.EmptyStorageRootHash)
    createProxiedContractStorageTrie(stateStorage, storageRoot)
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
  ): InMemoryWorldStateProxy =
    new InMemoryWorldStateProxy(
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
    )

  override def getBlockHash(number: UInt256): Option[UInt256] = getBlockByNumber(number).map(UInt256(_))

  /**
    * Returns an [[InMemorySimpleMapProxy]] of the contract storage, for `ethCompatibleStorage` defined as "trie as a map-ping from the Keccak
    * 256-bit hash of the 256-bit integer keys to the RLP-encoded256-bit integer values."
    * See [[http://paper.gavwood.com YP 4.1]]
    *
    * @param contractStorage Storage where trie nodes are saved
    * @param storageRoot     Trie root
    * @return Proxied Contract Storage Trie
    */
  private def createProxiedContractStorageTrie(
      contractStorage: MptStorage,
      storageRoot: ByteString
  ): InMemorySimpleMapProxy[BigInt, BigInt, MerklePatriciaTrie[BigInt, BigInt]] = {
    val mpt =
      if (ethCompatibleStorage) domain.EthereumUInt256Mpt.storageMpt(storageRoot, contractStorage)
      else domain.ArbitraryIntegerMpt.storageMpt(storageRoot, contractStorage)

    InMemorySimpleMapProxy.wrap[BigInt, BigInt, MerklePatriciaTrie[BigInt, BigInt]](mpt)
  }
}
