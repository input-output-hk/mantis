package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, Address, Block, Blockchain, UInt256}
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.proof.{
  ProofNode,
  StateTrieAccountValue,
  StorageProofKey,
  WorldStateProof,
  WorldStateProofProvider
}
import monix.eval.Task

import scala.annotation.tailrec

/**
  * Request to eth get proof
  *
  * @param address the address of the account or contract
  * @param storageKeys array of storage keys;
  *   a storage key is indexed from the solidity compiler by the order it is declared.
  *   For mappings it uses the keccak of the mapping key with its position (and recursively for X-dimensional mappings).
  *   See eth_getStorageAt
  * @param blockNumber block number (integer block number or string "latest", "earliest", ...)
  */
case class GetProofRequest(address: Address, storageKeys: Seq[StorageProofKey], blockNumber: BlockParam)

/**
  * Object proving a relationship of a storage value to an account's storageHash
  *
  * @param key storage proof key
  * @param value the value of the storage slot in its account tree
  * @param proof the set of node values needed to traverse a patricia merkle tree (from root to leaf) to retrieve a value
  */
case class StorageProof(
    key: StorageProofKey, // UInt256
    value: BigInt,
    proof: Seq[ProofNode] // List<Bytes>
)

/**
  * The merkle proofs of the specified account connecting them to the blockhash of the block specified.
  *
  * Proof of account consists of:
  * - account object: nonce, balance, storageHash, codeHash
  * - Markle Proof for the account starting with stateRoot from specified block
  * - Markle Proof for each requested storage entory starting with a storage Hash from the account
  *
  * @param address the address of the account or contract of the request
  * @param accountProof Markle Proof for the account starting with stateRoot from specified block
  * @param balance the Ether balance of the account or contract of the request
  * @param codeHash the code hash of the contract of the request (keccak(NULL) if external account)
  * @param nonce the transaction count of the account or contract of the request
  * @param storageHash the storage hash of the contract of the request (keccak(rlp(NULL)) if external account)
  * @param storageProof current block header PoW hash
  */
case class ProofAccount(
    address: Address,
    accountProof: Seq[ProofNode], // List<Bytes>
    balance: BigInt, // Wei
    codeHash: ByteString, // Bytes32
    nonce: UInt256,
    storageHash: ByteString, // Bytes32
    storageProof: Seq[StorageProof] // StorageEntryProof
) // besu GetProofResult

/**
  * spec: [EIP-1186](https://eips.ethereum.org/EIPS/eip-1186)
  * besu: https://github.com/PegaSysEng/pantheon/pull/1824/files
  * openethereum: https://github.com/openethereum/openethereum/pull/9001/files
  * go-ethereum: https://github.com/ethereum/go-ethereum/pull/17737/files
  */
class EthGetProof(blockchain: Blockchain, resolver: BlockResolver) {

  // TODO original impl, I trid to figure out how to do it using super unprecise desc in JSON RPC spec
  def apply(req: GetProofRequest): Task[Either[JsonRpcError, Option[ProofAccount]]] = Task {
    val bk: Option[Block] = resolver.resolveBlock(req.blockNumber).toOption.map(_.block)
    val maybeAccount = for {
      block <- bk
      account <- blockchain.getAccount(req.address, block.number)
      node <- blockchain.getMptNodeByHash(req.address.bytes)
    } yield ProofAccount(
      address = req.address,
      accountProof = getAccountProofOld(block, account, node, req.storageKeys),
      balance = account.balance,
      codeHash = account.codeHash,
      nonce = account.nonce,
      storageHash = account.storageRoot,
      storageProof = getStorageProof(block, account, node, req.storageKeys)
    )
    Right(maybeAccount)
  }

  def getStorageProof(
      block: Block,
      account: Account,
      node: MptNode,
      storageKeys: Seq[StorageProofKey]
  ): Seq[StorageProof] = {
    val worldsState = blockchain.getWorldStateProxy(
      block.number,
      account.nonce,
      None,
      false,
      false // TODO are those right?
    )

    // TODO current block header PoW hash
    val key: StorageProofKey = ???
    val value: BigInt = ???
    val proof: Seq[ProofNode] = ???
    StorageProof(key: StorageProofKey, value: BigInt, proof: Seq[ProofNode])
    ???
  }

  def getAccountProofOld(
      block: Block,
      account: Account,
      node: MptNode,
      storageKeys: Seq[StorageProofKey]
  ): Seq[ProofNode] = {
    @tailrec
    def getProofNodeFor(block: Block, soFar: Seq[ProofNode]): Seq[ProofNode] = {
      val hash: ByteString = block.header.parentHash
      val parent: Option[Block] = blockchain.getBlockByHash(hash)
      parent match {
        case Some(e) =>
          getProofNodeFor(e, soFar :+ ProofNode(hash))
        case None => soFar
      }
    }

    getProofNodeFor(block, Seq.empty)
  }

  // XXX new approach transalte from othere ETC clients
  // see org.hyperledger.besu.ethereum.api.jsonrpc.internal.methods.EthGetProof

  def resultByBlockNumber(req: GetProofRequest): Task[Either[JsonRpcError, Option[ProofAccount]]] = Task {
    val address: Address = req.address
    val storageKeys: Seq[StorageProofKey] = req.storageKeys
    val worldState: blockchain.WS = blockchain.getWorldStateProxy(
      blockNumber = ???, //req.blockNumber,
      accountStartNonce = ???,
      stateRootHash = ???,
      noEmptyAccounts = ???,
      ethCompatibleStorage = ???
    ) // TODO do we need a separae method? use blockResolver?

    val todoRootHash: Array[Byte] = ??? // TODO root has from world state

    val worldStateProof: Option[WorldStateProof] =
      getWorldStateArchive(blockchain).getAccountProof(todoRootHash, address, storageKeys)
    // TODO need to implement WSP creation at blockchain

    val asFailure: Either[JsonRpcError, Some[ProofAccount]] = Left(JsonRpcError.WorldStateUnavailable)
    worldStateProof.fold(asFailure) { proof => Right(Some(buildGetProofResult(address, proof))) }
  }

  def getWorldStateArchive(blockchain: Blockchain): WorldStateArchive = {
    ???
  }

  def buildGetProofResult(address: Address, worldStateProof: WorldStateProof): ProofAccount = {
    val stav: StateTrieAccountValue = worldStateProof.stateTrieAccountValue

    val storageProof: Seq[StorageProof] =
      worldStateProof
        .storageKeys()
        .map { key =>
          StorageProof(
            key = key,
            value = worldStateProof.storageValue(key),
            proof = worldStateProof.storageProof(key)
          )
        }
        .toSeq
    ProofAccount(
      address = address,
      accountProof = worldStateProof.getAccountProof.map(ProofNode.apply),
      balance = stav.balance,
      codeHash = stav.codeHash,
      nonce = stav.nonce,
      storageHash = stav.storageRoot,
      storageProof = storageProof
    )
  } // XXX besu GetProofResult buildGetProofResult
}

trait WorldStateArchive {

  def getAccountProof(
      rootHash: Array[Byte],
      address: Address,
      storageKeys: Seq[StorageProofKey]
  ): Option[WorldStateProof]
}

class DefaultWorldStateArchive(wspp: WorldStateProofProvider) extends WorldStateArchive {

  override def getAccountProof(
      rootHash: Array[Byte],
      address: Address,
      storageKeys: Seq[StorageProofKey]
  ): Option[WorldStateProof] = {
    wspp.getAccountProof(rootHash, address, storageKeys)
  }
}
