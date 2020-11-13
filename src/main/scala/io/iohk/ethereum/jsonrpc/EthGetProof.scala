package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import cats.implicits._
import io.iohk.ethereum.domain.{Account, Address, Blockchain, UInt256}
import io.iohk.ethereum.jsonrpc.EthService._
import monix.eval.Task

/** The key used to get the storage slot in its account tree */
final case class StorageProofKey(v: BigInt) extends AnyVal

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
    key: StorageProofKey,
    value: BigInt,
    proof: Seq[ByteString]
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
    accountProof: Seq[ByteString],
    balance: BigInt,
    codeHash: ByteString,
    nonce: UInt256,
    storageHash: ByteString,
    storageProof: Seq[StorageProof]
)

/**
  * spec: [EIP-1186](https://eips.ethereum.org/EIPS/eip-1186)
  * besu: https://github.com/PegaSysEng/pantheon/pull/1824/files
  * openethereum: https://github.com/openethereum/openethereum/pull/9001/files
  * go-ethereum: https://github.com/ethereum/go-ethereum/pull/17737/files
  */
class EthGetProof(blockchain: Blockchain, resolver: BlockResolver) {

  def run(req: GetProofRequest): Task[Either[JsonRpcError, ProofAccount]] = Task {
    val address = req.address
    for {
      blockNumber <- resolver.resolveBlock(req.blockNumber).map(_.block.number)
      account <- Either.fromOption(
        blockchain.getAccount(address, blockNumber),
        noAccount(address, blockNumber)
      )
      accountProof <- Either.fromOption(
        blockchain.getAccountProof(address, blockNumber),
        noAccountProof(address, blockNumber)
      )
      storageProof <- getStorageProof(account, req.storageKeys)
    } yield mkAccountProof(account, accountProof, storageProof, address)
  }

  def getStorageProof(
      account: Account,
      storageKeys: Seq[StorageProofKey]
  ): Either[JsonRpcError, Seq[StorageProof]] = {
    storageKeys.toList
      .map { storageKey =>
        blockchain
          .getStorageProofAt(
            rootHash = account.storageRoot,
            position = storageKey.v,
            ethCompatibleStorage = false
          )
          .map { case (value, proof) => StorageProof(storageKey, value, proof) }
          .toRight(noStorageProof(account, storageKey))
      }
      .sequence
      .map(_.toSeq)
  }

  private def noStorageProof(account: Account, storagekey: StorageProofKey): JsonRpcError =
    JsonRpcError.LogicError(s"No storage proof for [${account.toString}] storage key [${storagekey.toString}]")

  private def noAccount(address: Address, blockNumber: BigInt): JsonRpcError =
    JsonRpcError.LogicError(s"No storage proof for Address [${address.toString}] blockNumber [${blockNumber.toString}]")

  private def noAccountProof(address: Address, blockNumber: BigInt): JsonRpcError =
    JsonRpcError.LogicError(s"No storage proof for Address [${address.toString}] blockNumber [${blockNumber.toString}]")

  private def mkAccountProof(
      account: Account,
      accountProof: Seq[ByteString],
      storageProof: Seq[StorageProof],
      address: Address
  ): ProofAccount = {
    ProofAccount(
      address = address,
      accountProof = accountProof,
      balance = account.balance,
      codeHash = account.codeHash,
      nonce = account.nonce,
      storageHash = account.storageRoot,
      storageProof = storageProof
    )
  }
}
