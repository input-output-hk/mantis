package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import akka.util.ByteString
import monix.eval.Task
import io.iohk.ethereum.jsonrpc.EthService.ResolvedBlock
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException

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
    blockchain: Blockchain,
    ledger: Ledger,
    blockchainConfig: BlockchainConfig
) {
  import EthUserService._

  def getCode(req: GetCodeRequest): ServiceResponse[GetCodeResponse] = {
    Task {
      EthService
        .resolveBlock(blockchain, ledger, req.block)
        .map { case ResolvedBlock(block, _) =>
          val world = blockchain.getWorldStateProxy(
            block.header.number,
            blockchainConfig.accountStartNonce,
            block.header.stateRoot,
            noEmptyAccounts = false,
            ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
          )
          GetCodeResponse(world.getCode(req.address))
        }
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
      EthService
        .resolveBlock(blockchain, ledger, blockParam)
        .map { case ResolvedBlock(block, _) =>
          blockchain
            .getAccount(address, block.header.number)
            .getOrElse(Account.empty(blockchainConfig.accountStartNonce))
        }
        .map(makeResponse)
    }.onErrorRecover { case _: MissingNodeException =>
      Left(JsonRpcError.NodeNotFound)
    }

}
