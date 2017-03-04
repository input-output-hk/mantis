package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.common.SimpleMap
import io.iohk.ethereum.db.storage.EvmCodeStorage.{Code, CodeHash}
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt._
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPEncoder, decode => rlpDecode}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.vm.{DataWord, Storage, WorldStateProxy}
import io.iohk.ethereum.ledger.MPTWrappedMap._
import io.iohk.ethereum.ledger.EvmCodeStorageWrappedMap._
import io.iohk.ethereum.ledger.NodeStorageWrappedMap._
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.network.p2p.messages.PV63.AccountImplicits._

object InMemoryWorldStateProxy {

  //FIXME Move it to somewhere else
  implicit val dataWordRLPEncDec = new RLPEncoder[DataWord] with RLPDecoder[DataWord] {
    override def encode(obj: DataWord): RLPEncodeable = obj.bytes

    override def decode(rlp: RLPEncodeable): DataWord = DataWord(rlpDecode[ByteString](rlp))
  }

  val byteArrayDataWordSerializer = new ByteArrayEncoder[DataWord] {
    override def toBytes(input: DataWord): Array[Byte] = input.bytes.toArray[Byte]
  }

  val rlpDataWordSerializer = new RLPByteArraySerializable[DataWord]
  implicit val accountSerializer = new RLPByteArraySerializable[Account]

  def apply(
    storages: BlockchainStorages,
    stateStorage: NodeStorage,
    stateRootHash: ByteString): InMemoryWorldStateProxy = {

    val stateStorageProxy = InMemorySimpleMapProxy.wrap(stateStorage)
    val accountsStateTrieProxy = createProxiedAccountsStateTrie(stateStorageProxy, stateRootHash)
    val evmCodeStorageProxies = InMemorySimpleMapProxy.wrap(storages.evmCodeStorage)

    val getBlockHashByNumber = (number: BigInt) => BlockchainImpl(storages).getBlockHeaderByNumber(number).map(_.hash)

    new InMemoryWorldStateProxy(stateStorageProxy, accountsStateTrieProxy, Map.empty, evmCodeStorageProxies, Map.empty, getBlockHashByNumber)
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
  def commitState(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    def commitCode(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
      worldState.accountCodes.foldLeft(worldState) {
        case (updatedWorldState, (address, code)) =>
          val codeHash = kec256(code)
          updatedWorldState.copyWith(
            accountsStateTrie = updatedWorldState.accountsStateTrie +
              (address.bytes -> updatedWorldState.getGuaranteedAccount(address).copy(codeHash = codeHash)),
            evmCodeStorage = updatedWorldState.evmCodeStorage + (codeHash -> code),
            accountCodes = Map.empty
          )
      }
    }

    def commitContractStorage(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
      worldState.contractStorage.foldLeft(worldState) { case (updatedWorldState, (address, storageTrie)) =>
        val persistedStorage = storageTrie.persist()
        val newStorageRootHash = persistedStorage.inner.wrapped.getRootHash

        updatedWorldState.copyWith(
          accountsStateTrie = updatedWorldState.accountsStateTrie +
            (address.bytes -> updatedWorldState.getGuaranteedAccount(address).copy(storageRoot = ByteString(newStorageRootHash))),
          contractStorage = Map.empty
        )
      }

    def persistAccountsStateTrie(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
      worldState.copyWith(accountsStateTrie = worldState.accountsStateTrie.persist())

    (commitCode _ andThen commitContractStorage andThen persistAccountsStateTrie) (worldState)
  }

  /**
    * Persists the changes into the storages. This operation will actually write on disk and cannot be undone.
    *
    * @param worldState Proxy to persist
    * @return Updated world
    */
  def persist(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    def persistStorages(worldState: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
      worldState.copyWith(
        stateStorage = worldState.stateStorage.persist(),
        evmCodeStorage = worldState.evmCodeStorage.persist()
      )
    (commitState _ andThen persistStorages) (worldState)
  }


  /**
    * Returns an [[InMemorySimpleMapProxy]] of the accounts state trie "The world state (state), is a map-ping
    * between addresses (160-bit identifiers) and account states (a data structure serialised as RLP [...]).
    * Though not stored on the blockchain, it is assumed that the implementation will maintain this mapping in a
    * modified Merkle Patricia tree [...])."
    *
    * * See [[http://paper.gavwood.com YP 4.1]]
    *
    * @param accountsStorage Accounts Storage where trie nodes are saved
    * @param stateRootHash   State trie root hash
    * @return Proxied Accounts State Trie
    */
  private def createProxiedAccountsStateTrie(accountsStorage: SimpleMap[NodeHash, NodeEncoded], stateRootHash: ByteString)
  : InMemorySimpleMapProxy[ByteString, Account, MerklePatriciaTrie[ByteString, Account]] =
    InMemorySimpleMapProxy.wrap(
      MerklePatriciaTrie[ByteString, Account](
        stateRootHash.toArray[Byte],
        accountsStorage,
        kec256(_: Array[Byte])
      )
    )

  /**
    * Returns an [[InMemorySimpleMapProxy]] of the contract storage defined as "trie as a map-ping from the Keccak
    * 256-bit hash of the 256-bit integer keys to the RLP-encoded256-bit integer values."
    * See [[http://paper.gavwood.com YP 4.1]]
    *
    * @param contractStorage Storage where trie nodes are saved
    * @param storageRoot     Trie root
    * @return Proxied Contract Storage Trie
    */
  private def createProxiedContractStorageTrie(contractStorage: SimpleMap[NodeHash, NodeEncoded], storageRoot: ByteString):
  InMemorySimpleMapProxy[DataWord, DataWord, MerklePatriciaTrie[DataWord, DataWord]] =
    InMemorySimpleMapProxy.wrap(
      kec256SecuredTrie[DataWord, DataWord](storageRoot.toArray[Byte], contractStorage)(byteArrayDataWordSerializer, rlpDataWordSerializer)
    )
}

class InMemoryWorldStateProxyStorage(val wrapped: InMemorySimpleMapProxy[DataWord, DataWord, MerklePatriciaTrie[DataWord, DataWord]])
  extends Storage[InMemoryWorldStateProxyStorage] {

  override def store(addr: DataWord, value: DataWord): InMemoryWorldStateProxyStorage = new InMemoryWorldStateProxyStorage(wrapped.put(addr, value))

  override def load(addr: DataWord): DataWord = wrapped.get(addr).getOrElse(DataWord.Zero)
}

class InMemoryWorldStateProxy private(
  // State MPT proxied nodes storage needed to construct the storage MPT when calling [[getStorage]].
  // Accounts state and accounts storage states are saved within the same storage
  val stateStorage: InMemorySimpleMapProxy[NodeHash, NodeEncoded, NodeStorage],
  val accountsStateTrie: InMemorySimpleMapProxy[ByteString, Account, MerklePatriciaTrie[Code, Account]],
  // Contract Storage Proxies by Address
  val contractStorage: Map[Address, InMemorySimpleMapProxy[DataWord, DataWord, MerklePatriciaTrie[DataWord, DataWord]]],
  //It's easier to use the storage instead of the blockchain here (because of proxy wrapping). We might need to reconsider this
  val evmCodeStorage: InMemorySimpleMapProxy[CodeHash, Code, EvmCodeStorage],
  // Account's code by Address
  val accountCodes: Map[Address, Code],
  val getBlockByNumber: (BigInt) => Option[Code]
) extends WorldStateProxy[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage] {

  import InMemoryWorldStateProxy._

  override def getAccount(address: Address): Option[Account] = accountsStateTrie.get(address.bytes)

  override def getGuaranteedAccount(address: Address): Account = super.getGuaranteedAccount(address)

  override def saveAccount(address: Address, account: Account): InMemoryWorldStateProxy =
    copyWith(accountsStateTrie = accountsStateTrie.put(address.bytes, account))

  override def getCode(address: Address): ByteString =
    accountCodes.getOrElse(
      address,
      getAccount(address).flatMap(account => evmCodeStorage.get(account.codeHash)).getOrElse(ByteString.empty)
    )

  override def getStorage(address: Address): InMemoryWorldStateProxyStorage =
    new InMemoryWorldStateProxyStorage(contractStorage.getOrElse(address, getStorageForAddress(address, stateStorage)))

  override def saveCode(address: Address, code: ByteString): InMemoryWorldStateProxy =
    copyWith(accountCodes = accountCodes + (address -> code))


  override def saveStorage(address: Address, storage: InMemoryWorldStateProxyStorage): InMemoryWorldStateProxy =
    copyWith(contractStorage = contractStorage + (address -> storage.wrapped))

  def stateRoothash: ByteString = ByteString(accountsStateTrie.inner.wrapped.getRootHash)

  private def getStorageForAddress(address: Address, stateStorage: InMemorySimpleMapProxy[NodeHash, NodeEncoded, NodeStorage]) = {
    val storageRoot = getAccount(address)
      .map(account => account.storageRoot)
      .getOrElse(Account.EmptyStorageRootHash)
    createProxiedContractStorageTrie(stateStorage, storageRoot)
  }

  private def copyWith(
    stateStorage: InMemorySimpleMapProxy[NodeHash, NodeEncoded, NodeStorage] = stateStorage,
    accountsStateTrie: InMemorySimpleMapProxy[ByteString, Account, MerklePatriciaTrie[Code, Account]] = accountsStateTrie,
    contractStorage: Map[Address, InMemorySimpleMapProxy[DataWord, DataWord, MerklePatriciaTrie[DataWord, DataWord]]] = contractStorage,
    evmCodeStorage: InMemorySimpleMapProxy[CodeHash, Code, EvmCodeStorage] = evmCodeStorage,
    accountCodes: Map[Address, Code] = accountCodes
  ): InMemoryWorldStateProxy =
    new InMemoryWorldStateProxy(
      stateStorage,
      accountsStateTrie,
      contractStorage,
      evmCodeStorage,
      accountCodes,
      getBlockByNumber
    )

  override def getBlockHash(number: BigInt): Option[ByteString] = getBlockByNumber(number)
}

