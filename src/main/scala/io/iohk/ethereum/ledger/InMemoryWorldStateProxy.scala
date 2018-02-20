package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.EvmCodeStorage.Code
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, _}
import io.iohk.ethereum.vm.{Storage, WorldStateProxy}

object InMemoryWorldStateProxy {

  import Account._

  def apply[S <: Storage[S, T], T](
    evmCodeStorage: EvmCodeStorage,
    nodesKeyValueStorage: NodesKeyValueStorage,
    contractStorageHandler: ContractStorageHandler[S, T],
    accountStartNonce: UInt256,
    getBlockHashByNumber: BigInt => Option[ByteString],
    stateRootHash: Option[ByteString] = None,
    noEmptyAccounts: Boolean
  ): InMemoryWorldStateProxy[S, T] = {
    val accountsStateTrieProxy = createProxiedAccountsStateTrie(
      nodesKeyValueStorage,
      stateRootHash.getOrElse(ByteString(MerklePatriciaTrie.EmptyRootHash))
    )
    new InMemoryWorldStateProxy(
      nodesKeyValueStorage,
      accountsStateTrieProxy,
      contractStorageHandler,
      Map.empty,
      evmCodeStorage,
      Map.empty,
      getBlockHashByNumber,
      accountStartNonce,
      Set.empty,
      noEmptyAccounts
    )
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
  private def createProxiedAccountsStateTrie(accountsStorage: NodesKeyValueStorage, stateRootHash: ByteString)
  : InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]] = {
    InMemorySimpleMapProxy.wrap[Address, Account, MerklePatriciaTrie[Address, Account]](
      MerklePatriciaTrie[Address, Account](
        stateRootHash.toArray[Byte],
        accountsStorage
      )(Address.hashedAddressEncoder, accountSerializer)
    )
  }
}

trait ContractStorageHandler[S <: Storage[S, T], T] {
  def build(rootHash: ByteString, nodeStorage: NodesKeyValueStorage): S
  def persist(storage: S): S
  def rootHash(storage: S): ByteString
}

object UInt256StorageHandler extends ContractStorageHandler[UInt256Storage, UInt256] {
  import io.iohk.ethereum.rlp.UInt256RLPImplicits._

  val byteArrayUInt256Serializer = new ByteArrayEncoder[UInt256] {
    override def toBytes(input: UInt256): Array[Byte] = input.bytes.toArray[Byte]
  }

  val rlpUInt256Serializer = new ByteArraySerializable[UInt256] {
    override def fromBytes(bytes: Array[Byte]): UInt256 = ByteString(bytes).toUInt256
    override def toBytes(input: UInt256): Array[Byte] = input.toBytes
  }

  def storageMpt(rootHash: ByteString, nodeStorage: NodesKeyValueStorage): MerklePatriciaTrie[UInt256, UInt256] =
    MerklePatriciaTrie[UInt256, UInt256](rootHash.toArray[Byte], nodeStorage)(HashByteArraySerializable(byteArrayUInt256Serializer), rlpUInt256Serializer)

  def build(rootHash: ByteString, nodeStorage: NodesKeyValueStorage): UInt256Storage = {
    val mpt = storageMpt(rootHash, nodeStorage)
    val map = InMemorySimpleMapProxy.wrap[UInt256, UInt256, MerklePatriciaTrie[UInt256, UInt256]](mpt)
    UInt256Storage(map)
  }

  def persist(storage: UInt256Storage): UInt256Storage =
    UInt256Storage(storage.wrapped.persist())

  def rootHash(storage: UInt256Storage): ByteString =
    ByteString(storage.wrapped.inner.getRootHash)
}

case class UInt256Storage(wrapped: InMemorySimpleMapProxy[UInt256, UInt256, MerklePatriciaTrie[UInt256, UInt256]])
  extends Storage[UInt256Storage, UInt256] {

  override def store(addr: UInt256, value: UInt256): UInt256Storage = {
    val newWrapped =
      if(value.isZero) wrapped - addr
      else wrapped + (addr -> value)
    UInt256Storage(newWrapped)
  }

  override def load(addr: UInt256): UInt256 = wrapped.get(addr).getOrElse(UInt256.Zero)
}

object BigIntStorageHandler extends ContractStorageHandler[BigIntStorage, BigInt] {

  val bigIntSerializer = new ByteArraySerializable[BigInt] {
    override def fromBytes(bytes: Array[Byte]): BigInt = BigInt(bytes)
    override def toBytes(input: BigInt): Array[Byte] = input.toByteArray
  }

  def build(rootHash: ByteString, nodeStorage: NodesKeyValueStorage): BigIntStorage = {
    val mpt = MerklePatriciaTrie[BigInt, BigInt](rootHash.toArray[Byte], nodeStorage)(HashByteArraySerializable(bigIntSerializer), bigIntSerializer)
    val map = InMemorySimpleMapProxy.wrap[BigInt, BigInt, MerklePatriciaTrie[BigInt, BigInt]](mpt)
    BigIntStorage(map)
  }

  def persist(storage: BigIntStorage): BigIntStorage =
    BigIntStorage(storage.wrapped.persist())

  def rootHash(storage: BigIntStorage): ByteString =
    ByteString(storage.wrapped.inner.getRootHash)
}

case class BigIntStorage(wrapped: InMemorySimpleMapProxy[BigInt, BigInt, MerklePatriciaTrie[BigInt, BigInt]])
  extends Storage[BigIntStorage, BigInt] {

  override def store(addr: BigInt, value: BigInt): BigIntStorage = {
    val newWrapped =
      if (value == 0) wrapped - addr
      else wrapped + (addr -> value)
    BigIntStorage(newWrapped)
  }

  override def load(addr: BigInt): BigInt = wrapped.get(addr).getOrElse(0)
}

class InMemoryWorldStateProxy[S <: Storage[S, T], T] private[ledger](
  // State MPT proxied nodes storage needed to construct the storage MPT when calling [[getStorage]].
  // Accounts state and accounts storage states are saved within the same storage
  val stateStorage: NodesKeyValueStorage,
  val accountsStateTrie: InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]],
  // Contract Storage Proxies by Address
  val contractStorageHandler: ContractStorageHandler[S, T],
  val contractStorages: Map[Address, S],
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
  val noEmptyAccountsCond: Boolean
) extends WorldStateProxy[InMemoryWorldStateProxy[S, T], S, T] {

  import InMemoryWorldStateProxy._

  override def getAccount(address: Address): Option[Account] = accountsStateTrie.get(address)

  override def getEmptyAccount: Account = Account.empty(accountStartNonce)

  override def getGuaranteedAccount(address: Address): Account = super.getGuaranteedAccount(address)

  override def saveAccount(address: Address, account: Account): InMemoryWorldStateProxy[S, T] = {
    copyWith(accountsStateTrie = accountsStateTrie.put(address, account))
  }

  override def deleteAccount(address: Address): InMemoryWorldStateProxy[S, T] =
    copyWith(accountsStateTrie = accountsStateTrie.remove(address),
      contractStorages = contractStorages - address,
      accountCodes = accountCodes - address)

  override def getCode(address: Address): ByteString =
    accountCodes.getOrElse(
      address,
      getAccount(address).flatMap(account => evmCodeStorage.get(account.codeHash)).getOrElse(ByteString.empty)
    )

  override def getStorage(address: Address): S =
    contractStorages.getOrElse(address, getStorageForAddress(address, stateStorage))

  override def saveCode(address: Address, code: ByteString): InMemoryWorldStateProxy[S, T] =
    copyWith(accountCodes = accountCodes + (address -> code))


  override def saveStorage(address: Address, storage: S): InMemoryWorldStateProxy[S, T] =
    copyWith(contractStorages = contractStorages + (address -> storage))

  override def touchAccounts(addresses: Address*): InMemoryWorldStateProxy[S, T] =
    if (noEmptyAccounts)
      copyWith(touchedAccounts = touchedAccounts ++ addresses.toSet)
    else
      this

  override def clearTouchedAccounts: InMemoryWorldStateProxy[S, T] =
    copyWith(touchedAccounts = touchedAccounts.empty)

  override def noEmptyAccounts: Boolean = noEmptyAccountsCond

  override def combineTouchedAccounts(world: InMemoryWorldStateProxy[S, T]): InMemoryWorldStateProxy[S, T] = {
    copyWith(touchedAccounts = touchedAccounts ++ world.touchedAccounts)
  }

  /**
    * Returns world state root hash. This value is only updated after persist.
    */
  def stateRootHash: ByteString = ByteString(accountsStateTrie.inner.getRootHash)

  private def getStorageForAddress(address: Address, stateStorage: NodesKeyValueStorage): S = {
    val storageRoot = getAccount(address)
      .map(account => account.storageRoot)
      .getOrElse(Account.EmptyStorageRootHash)
    contractStorageHandler.build(storageRoot, stateStorage)
  }

  private def copyWith(
    stateStorage: NodesKeyValueStorage = stateStorage,
    accountsStateTrie: InMemorySimpleMapProxy[Address, Account, MerklePatriciaTrie[Address, Account]] = accountsStateTrie,
    contractStorages: Map[Address, S] = contractStorages,
    evmCodeStorage: EvmCodeStorage = evmCodeStorage,
    accountCodes: Map[Address, Code] = accountCodes,
    touchedAccounts: Set[Address] = touchedAccounts
  ): InMemoryWorldStateProxy[S, T] =
    new InMemoryWorldStateProxy(
      stateStorage,
      accountsStateTrie,
      contractStorageHandler,
      contractStorages,
      evmCodeStorage,
      accountCodes,
      getBlockByNumber,
      accountStartNonce,
      touchedAccounts,
      noEmptyAccountsCond
    )

  override def getBlockHash(number: UInt256): Option[UInt256] = getBlockByNumber(number).map(UInt256(_))


  /**
    * Updates state trie with current changes but does not persist them into the storages. To do so it:
    *   - Commits code (to get account's code hashes)
    *   - Commits constract storages (to get account's contract storage root)
    *   - Updates state tree
    *
    * @return Updated world
    */
  def persistState(): InMemoryWorldStateProxy[S, T] = {
    def persistCode(worldState: InMemoryWorldStateProxy[S, T]): InMemoryWorldStateProxy[S, T] = {
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

    def persistContractStorage(worldState: InMemoryWorldStateProxy[S, T]): InMemoryWorldStateProxy[S, T] =
      worldState.contractStorages.foldLeft(worldState) { case (updatedWorldState, (address, storage)) =>
        val persistedStorage = contractStorageHandler.persist(storage)
        val newStorageRootHash = contractStorageHandler.rootHash(persistedStorage)

        updatedWorldState.copyWith(
          contractStorages = updatedWorldState.contractStorages + (address -> persistedStorage),
          accountsStateTrie = updatedWorldState.accountsStateTrie +
            (address -> updatedWorldState.getGuaranteedAccount(address).copy(storageRoot = ByteString(newStorageRootHash)))
        )
      }

    def persistAccountsStateTrie(worldState: InMemoryWorldStateProxy[S, T]): InMemoryWorldStateProxy[S, T] =
      worldState.copyWith(accountsStateTrie = worldState.accountsStateTrie.persist())

    (persistCode _ andThen persistContractStorage andThen persistAccountsStateTrie) (this)
  }
}

