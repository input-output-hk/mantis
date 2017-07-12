package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.EvmCodeStorage.Code
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, _}
import io.iohk.ethereum.vm.{Storage, UInt256, WorldStateProxy}

object InMemoryWorldStateProxy {

  import Account._

  def apply(
    storages: BlockchainStorages,
    accountStartNonce: UInt256,
    stateRootHash: Option[ByteString] = None): InMemoryWorldStateProxy = {

    val accountsStateTrieProxy = createProxiedAccountsStateTrie(
      storages.nodeStorage,
      stateRootHash.getOrElse(ByteString(MerklePatriciaTrie.calculateEmptyRootHash(kec256(_: Array[Byte]))))
    )
    val blockchain = BlockchainImpl(storages)
    val getBlockHashByNumber = (number: BigInt) => blockchain.getBlockHeaderByNumber(number).map(_.hash)

    new InMemoryWorldStateProxy(storages.nodeStorage, accountsStateTrieProxy, Map.empty, storages.evmCodeStorage,
      Map.empty, getBlockHashByNumber, accountStartNonce)
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
  private[ledger] def persistState(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    def persistCode(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
      worldState.accountCodes.foldLeft(worldState) {
        case (updatedWorldState, (address, code)) =>
          val codeHash = kec256(code)
          updatedWorldState.copyWith(
            accountsStateTrie = updatedWorldState.accountsStateTrie +
              (address -> updatedWorldState.getGuaranteedAccount(address).copy(codeHash = codeHash)),
            evmCodeStorage = updatedWorldState.evmCodeStorage + (codeHash -> code),
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
            (address -> updatedWorldState.getGuaranteedAccount(address).copy(storageRoot = ByteString(newStorageRootHash)))
        )
      }

    def persistAccountsStateTrie(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
      worldState.copyWith(accountsStateTrie = worldState.accountsStateTrie.persist())

    (persistCode _ andThen persistContractStorage andThen persistAccountsStateTrie) (worldState)
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
  private def createProxiedAccountsStateTrie(accountsStorage: NodeStorage, stateRootHash: ByteString)
  : InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]] = {
    InMemorySimpleMapProxy.wrap[Address, Account, MerklePatriciaTrie[Address, Account]](
      MerklePatriciaTrie[Address, Account](
        stateRootHash.toArray[Byte],
        accountsStorage,
        kec256(_: Array[Byte])
      )(Address.hashedAddressEncoder, accountSerializer)
    )
  }

  /**
    * Returns an [[InMemorySimpleMapProxy]] of the contract storage defined as "trie as a map-ping from the Keccak
    * 256-bit hash of the 256-bit integer keys to the RLP-encoded256-bit integer values."
    * See [[http://paper.gavwood.com YP 4.1]]
    *
    * @param contractStorage Storage where trie nodes are saved
    * @param storageRoot     Trie root
    * @return Proxied Contract Storage Trie
    */
  private def createProxiedContractStorageTrie(contractStorage: NodeStorage, storageRoot: ByteString):
  InMemorySimpleMapProxy[UInt256, UInt256, MerklePatriciaTrie[UInt256, UInt256]] =
    InMemorySimpleMapProxy.wrap[UInt256, UInt256, MerklePatriciaTrie[UInt256, UInt256]](domain.storageMpt(storageRoot, contractStorage))
}

class InMemoryWorldStateProxyStorage(val wrapped: InMemorySimpleMapProxy[UInt256, UInt256, MerklePatriciaTrie[UInt256, UInt256]])
  extends Storage[InMemoryWorldStateProxyStorage] {

  override def store(addr: UInt256, value: UInt256): InMemoryWorldStateProxyStorage = {
    val newWrapped =
      if(value.isZero) wrapped - addr
      else wrapped + (addr -> value)
    new InMemoryWorldStateProxyStorage(newWrapped)
  }

  override def load(addr: UInt256): UInt256 = wrapped.get(addr).getOrElse(UInt256.Zero)
}

class InMemoryWorldStateProxy private(
  // State MPT proxied nodes storage needed to construct the storage MPT when calling [[getStorage]].
  // Accounts state and accounts storage states are saved within the same storage
  val stateStorage: NodeStorage,
  val accountsStateTrie: InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]],
  // Contract Storage Proxies by Address
  val contractStorages: Map[Address, InMemorySimpleMapProxy[UInt256, UInt256, MerklePatriciaTrie[UInt256, UInt256]]],
  //It's easier to use the storage instead of the blockchain here (because of proxy wrapping). We might need to reconsider this
  val evmCodeStorage: EvmCodeStorage,
  // Account's code by Address
  val accountCodes: Map[Address, Code],
  val getBlockByNumber: (BigInt) => Option[ByteString],
  accountStartNonce: UInt256
) extends WorldStateProxy[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage] {

  import InMemoryWorldStateProxy._

  override def getAccount(address: Address): Option[Account] = accountsStateTrie.get(address)

  override protected def getEmptyAccount: Account = Account.empty(accountStartNonce)

  override def getGuaranteedAccount(address: Address): Account = super.getGuaranteedAccount(address)

  override def saveAccount(address: Address, account: Account): InMemoryWorldStateProxy =
    copyWith(accountsStateTrie = accountsStateTrie.put(address, account))

  override def deleteAccount(address: Address): InMemoryWorldStateProxy =
    copyWith(accountsStateTrie = accountsStateTrie.remove(address),
      contractStorages = contractStorages - address,
      accountCodes = accountCodes - address)

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

  /**
    * Returns world state root hash. This value is only updated after persist.
    */
  def stateRootHash: ByteString = ByteString(accountsStateTrie.inner.getRootHash)

  private def getStorageForAddress(address: Address, stateStorage: NodeStorage) = {
    val storageRoot = getAccount(address)
      .map(account => account.storageRoot)
      .getOrElse(Account.EmptyStorageRootHash)
    createProxiedContractStorageTrie(stateStorage, storageRoot)
  }

  private def copyWith(
    stateStorage: NodeStorage = stateStorage,
    accountsStateTrie: InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]] = accountsStateTrie,
    contractStorages: Map[Address, InMemorySimpleMapProxy[UInt256, UInt256, MerklePatriciaTrie[UInt256, UInt256]]] = contractStorages,
    evmCodeStorage: EvmCodeStorage = evmCodeStorage,
    accountCodes: Map[Address, Code] = accountCodes
  ): InMemoryWorldStateProxy =
    new InMemoryWorldStateProxy(
      stateStorage,
      accountsStateTrie,
      contractStorages,
      evmCodeStorage,
      accountCodes,
      getBlockByNumber,
      accountStartNonce
    )

  override def getBlockHash(number: UInt256): Option[UInt256] = getBlockByNumber(number).map(UInt256(_))
}

