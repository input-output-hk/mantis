package io.iohk.ethereum.jsonrpc

import akka.util.ByteString

import monix.eval.Task

import io.iohk.ethereum.consensus.mining.Mining
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.nodebuilder.BlockchainConfigBuilder

object EthUserService {
  case class GetStorageAtRequest(address: Address, position: BigInt, block: BlockParam)
  case class GetStorageAtResponse(value: ByteString)
  case class GetCodeRequest(address: Address, block: BlockParam)
  case class GetCodeResponse(result: ByteString)
  case class GetBalanceRequest(address: Address, block: BlockParam)
  case class GetBalanceResponse(value: BigInt)
  case class GetTransactionCountRequest(address: Address, block: BlockParam)
  case class GetTransactionCountResponse(value: BigInt)
  case class GetStorageRootRequest(address: Address, block: BlockParam)
  case class GetStorageRootResponse(storageRoot: ByteString)
}

class EthUserService(
    val blockchain: Blockchain,
    val blockchainReader: BlockchainReader,
    val mining: Mining,
    evmCodeStorage: EvmCodeStorage,
    configBuilder: BlockchainConfigBuilder
) extends ResolveBlock {
  import configBuilder._
  import EthUserService._

  def getCode(req: GetCodeRequest): ServiceResponse[GetCodeResponse] =
    Task {
      resolveBlock(req.block).map { case ResolvedBlock(block, _) =>
        val world = InMemoryWorldStateProxy(
          evmCodeStorage,
          blockchain.getBackingMptStorage(block.header.number),
          (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
          blockchainConfig.accountStartNonce,
          block.header.stateRoot,
          noEmptyAccounts = false,
          ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
        )
        GetCodeResponse(world.getCode(req.address))
      }
    }

  def getBalance(req: GetBalanceRequest): ServiceResponse[GetBalanceResponse] =
    withAccount(req.address, req.block) { account =>
      GetBalanceResponse(account.balance)
    }

  def getStorageAt(req: GetStorageAtRequest): ServiceResponse[GetStorageAtResponse] =
    withAccount(req.address, req.block) { account =>
      GetStorageAtResponse(
        blockchain.getAccountStorageAt(account.storageRoot, req.position, blockchainConfig.ethCompatibleStorage)
      )
    }

  def getTransactionCount(req: GetTransactionCountRequest): ServiceResponse[GetTransactionCountResponse] =
    withAccount(req.address, req.block) { account =>
      GetTransactionCountResponse(account.nonce)
    }

  def getStorageRoot(req: GetStorageRootRequest): ServiceResponse[GetStorageRootResponse] =
    withAccount(req.address, req.block) { account =>
      GetStorageRootResponse(account.storageRoot)
    }

  private def withAccount[T](address: Address, blockParam: BlockParam)(makeResponse: Account => T): ServiceResponse[T] =
    Task {
      resolveBlock(blockParam)
        .map { case ResolvedBlock(block, _) =>
          blockchainReader
            .getAccount(blockchainReader.getBestBranch(), address, block.header.number)
            .getOrElse(Account.empty(blockchainConfig.accountStartNonce))
        }
        .map(makeResponse)
    }.onErrorRecover { case _: MissingNodeException =>
      Left(JsonRpcError.NodeNotFound)
    }

}
