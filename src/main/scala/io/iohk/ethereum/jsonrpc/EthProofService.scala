package io.iohk.ethereum.jsonrpc

import akka.util.ByteString

import cats.implicits._

import monix.eval.Task

import io.iohk.ethereum.consensus.blocks.BlockGenerator
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.jsonrpc.ProofService.GetProofRequest
import io.iohk.ethereum.jsonrpc.ProofService.GetProofResponse
import io.iohk.ethereum.jsonrpc.ProofService.ProofAccount
import io.iohk.ethereum.jsonrpc.ProofService.StorageProof
import io.iohk.ethereum.jsonrpc.ProofService.StorageProof.asRlpSerializedNode
import io.iohk.ethereum.jsonrpc.ProofService.StorageProofKey
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.mpt.MptTraversals

object ProofService {

  /** Request to eth get proof
    *
    * @param address     the address of the account or contract
    * @param storageKeys array of storage keys;
    *                    a storage key is indexed from the solidity compiler by the order it is declared.
    *                    For mappings it uses the keccak of the mapping key with its position (and recursively for X-dimensional mappings).
    *                    See eth_getStorageAt
    * @param blockNumber block number (integer block number or string "latest", "earliest", ...)
    */
  case class GetProofRequest(address: Address, storageKeys: Seq[StorageProofKey], blockNumber: BlockParam)

  case class GetProofResponse(proofAccount: ProofAccount)

  sealed trait StorageProof {
    def key: StorageProofKey
    def value: BigInt
    def proof: Seq[ByteString]
  }

  object StorageProof {
    def apply(position: BigInt, value: Option[BigInt], proof: Option[Vector[MptNode]]): StorageProof =
      (value, proof) match {
        case (Some(value), Some(proof)) =>
          StorageValueProof(StorageProofKey(position), value, proof.map(asRlpSerializedNode))
        case (None, Some(proof)) =>
          EmptyStorageValue(StorageProofKey(position), proof.map(asRlpSerializedNode))
        case (Some(value), None) => EmptyStorageProof(StorageProofKey(position), value)
        case (None, None)        => EmptyStorageValueProof(StorageProofKey(position))
      }

    def asRlpSerializedNode(node: MptNode): ByteString =
      ByteString(MptTraversals.encodeNode(node))
  }

  /** Object proving a relationship of a storage value to an account's storageHash
    *
    * @param key   storage proof key
    * @param value the value of the storage slot in its account tree
    * @param proof the set of node values needed to traverse a patricia merkle tree (from root to leaf) to retrieve a value
    */
  case class EmptyStorageValueProof(key: StorageProofKey) extends StorageProof {
    val value: BigInt = BigInt(0)
    val proof: Seq[ByteString] = Seq.empty[MptNode].map(asRlpSerializedNode)
  }
  case class EmptyStorageValue(key: StorageProofKey, proof: Seq[ByteString]) extends StorageProof {
    val value: BigInt = BigInt(0)
  }
  case class EmptyStorageProof(key: StorageProofKey, value: BigInt) extends StorageProof {
    val proof: Seq[ByteString] = Seq.empty[MptNode].map(asRlpSerializedNode)
  }
  case class StorageValueProof(key: StorageProofKey, value: BigInt, proof: Seq[ByteString]) extends StorageProof

  /** The key used to get the storage slot in its account tree */
  case class StorageProofKey(v: BigInt) extends AnyVal

  /** The merkle proofs of the specified account connecting them to the blockhash of the block specified.
    *
    * Proof of account consists of:
    * - account object: nonce, balance, storageHash, codeHash
    * - Markle Proof for the account starting with stateRoot from specified block
    * - Markle Proof for each requested storage entry starting with a storage Hash from the account
    *
    * @param address      the address of the account or contract of the request
    * @param accountProof Markle Proof for the account starting with stateRoot from specified block
    * @param balance      the Ether balance of the account or contract of the request
    * @param codeHash     the code hash of the contract of the request (keccak(NULL) if external account)
    * @param nonce        the transaction count of the account or contract of the request
    * @param storageHash  the storage hash of the contract of the request (keccak(rlp(NULL)) if external account)
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

  object ProofAccount {

    def apply(
        account: Account,
        accountProof: Seq[ByteString],
        storageProof: Seq[StorageProof],
        address: Address
    ): ProofAccount =
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

  sealed trait MptProofError
  object MptProofError {
    case object UnableRebuildMpt extends MptProofError
    case object KeyNotFoundInRebuidMpt extends MptProofError
  }
}

trait ProofService {

  /** Returns the account- and storage-values of the specified account including the Merkle-proof.
    */
  def getProof(req: GetProofRequest): ServiceResponse[GetProofResponse]
}

/** Spec: [EIP-1186](https://eips.ethereum.org/EIPS/eip-1186)
  * besu: https://github.com/PegaSysEng/pantheon/pull/1824/files
  * parity: https://github.com/openethereum/parity-ethereum/pull/9001
  * geth: https://github.com/ethereum/go-ethereum/pull/17737
  */
class EthProofService(
    blockchain: Blockchain,
    blockchainReader: BlockchainReader,
    blockGenerator: BlockGenerator,
    ethCompatibleStorage: Boolean
) extends ProofService {

  def getProof(req: GetProofRequest): ServiceResponse[GetProofResponse] =
    getProofAccount(req.address, req.storageKeys, req.blockNumber)
      .map(_.map(GetProofResponse.apply))

  /** Get account and storage values for account including Merkle Proof.
    *
    * @param address address of the account
    * @param storageKeys storage keys which should be proofed and included
    * @param block block number or string "latest", "earliest"
    * @return
    */
  def getProofAccount(
      address: Address,
      storageKeys: Seq[StorageProofKey],
      block: BlockParam
  ): Task[Either[JsonRpcError, ProofAccount]] = Task {
    for {
      blockNumber <- resolveBlock(block).map(_.block.number)
      account <- Either.fromOption(
        blockchain.getAccount(address, blockNumber),
        noAccount(address, blockNumber)
      )
      accountProof <- Either.fromOption(
        blockchain.getAccountProof(address, blockNumber).map(_.map(asRlpSerializedNode)),
        noAccountProof(address, blockNumber)
      )
      storageProof = getStorageProof(account, storageKeys)
    } yield ProofAccount(account, accountProof, storageProof, address)
  }

  def getStorageProof(
      account: Account,
      storageKeys: Seq[StorageProofKey]
  ): Seq[StorageProof] =
    storageKeys.toList
      .map { storageKey =>
        blockchain
          .getStorageProofAt(
            rootHash = account.storageRoot,
            position = storageKey.v,
            ethCompatibleStorage = ethCompatibleStorage
          )
      }

  private def noAccount(address: Address, blockNumber: BigInt): JsonRpcError =
    JsonRpcError.LogicError(s"No account found for Address [${address.toString}] blockNumber [${blockNumber.toString}]")

  private def noAccountProof(address: Address, blockNumber: BigInt): JsonRpcError =
    JsonRpcError.LogicError(s"No account proof for Address [${address.toString}] blockNumber [${blockNumber.toString}]")

  private def asRlpSerializedNode(node: MptNode): ByteString =
    ByteString(MptTraversals.encodeNode(node))

  private def resolveBlock(blockParam: BlockParam): Either[JsonRpcError, ResolvedBlock] = {
    def getBlock(number: BigInt): Either[JsonRpcError, Block] =
      blockchainReader
        .getBestBranch()
        .getBlockByNumber(number)
        .toRight(JsonRpcError.InvalidParams(s"Block $number not found"))

    def getLatestBlock(): Either[JsonRpcError, Block] =
      blockchainReader
        .getBestBlock()
        .toRight(JsonRpcError.InvalidParams("Latest block not found"))

    blockParam match {
      case BlockParam.WithNumber(blockNumber) => getBlock(blockNumber).map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Earliest                => getBlock(0).map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Latest                  => getLatestBlock().map(ResolvedBlock(_, pendingState = None))
      case BlockParam.Pending =>
        blockGenerator.getPendingBlockAndState
          .map(pb => ResolvedBlock(pb.pendingBlock.block, pendingState = Some(pb.worldState)))
          .map(Right.apply)
          .getOrElse(resolveBlock(BlockParam.Latest)) //Default behavior in other clients
    }
  }
}
