package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.fast.SyncStateScheduler.SyncResponse
import io.iohk.ethereum.domain.{Account, Address, Blockchain, BlockchainImpl}
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils.{BlockchainConfig, ByteUtils}

object StateSyncUtils extends EphemBlockchainTestSetup {

  final case class MptNodeData(
      accountAddress: Address,
      accountCode: Option[ByteString],
      accountStorage: Seq[(BigInt, BigInt)],
      accountBalance: Int
  )

  class TrieProvider(bl: Blockchain, blockchainConfig: BlockchainConfig) {
    def getNodes(hashes: List[ByteString]) = {
      hashes.map { hash =>
        val maybeResult = bl.getMptNodeByHash(hash) match {
          case Some(value) => Some(ByteString(value.encode))
          case None        => bl.getEvmCodeByHash(hash)
        }
        maybeResult match {
          case Some(result) => SyncResponse(hash, result)
          case None         => throw new RuntimeException("Missing expected data in storage")
        }
      }
    }

    def buildWorld(accountData: Seq[MptNodeData], existingTree: Option[ByteString] = None): ByteString = {
      val init: InMemoryWorldStateProxy = bl
        .getWorldStateProxy(
          blockNumber = 1,
          accountStartNonce = blockchainConfig.accountStartNonce,
          stateRootHash = existingTree.getOrElse(ByteString(MerklePatriciaTrie.EmptyRootHash)),
          noEmptyAccounts = true,
          ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
        )
        .asInstanceOf[InMemoryWorldStateProxy]

      val modifiedWorld = accountData.foldLeft(init) { case (world, data) =>
        val storage = world.getStorage(data.accountAddress)
        val modifiedStorage = data.accountStorage.foldLeft(storage) { case (s, v) =>
          s.store(v._1, v._2)
        }
        val code = world.getCode(data.accountAddress)
        val worldWithAccAndStorage = world
          .saveAccount(data.accountAddress, Account.empty().copy(balance = data.accountBalance))
          .saveStorage(data.accountAddress, modifiedStorage)

        val finalWorld =
          if (data.accountCode.isDefined)
            worldWithAccAndStorage.saveCode(data.accountAddress, data.accountCode.get)
          else
            worldWithAccAndStorage
        finalWorld
      }

      val persisted = InMemoryWorldStateProxy.persistState(modifiedWorld)
      persisted.stateRootHash
    }
  }

  object TrieProvider {
    def apply(): TrieProvider = {
      val freshStorage = getNewStorages
      new TrieProvider(BlockchainImpl(freshStorage.storages), blockchainConfig)
    }
  }

  def createNodeDataStartingFrom(initialNumber: Int, lastNumber: Int, storageOffset: Int): Seq[MptNodeData] = {
    (initialNumber until lastNumber).map { i =>
      val address = Address(i)
      val codeBytes = ByteString(BigInt(i).toByteArray)
      val storage = (initialNumber until initialNumber + storageOffset).map(s => (BigInt(s), BigInt(s)))
      val balance = i
      MptNodeData(address, Some(codeBytes), storage, balance)
    }
  }

  def checkAllDataExists(nodeData: List[MptNodeData], bl: Blockchain, blNumber: BigInt): Boolean = {
    def go(remaining: List[MptNodeData]): Boolean = {
      if (remaining.isEmpty) {
        true
      } else {
        val dataToCheck = remaining.head
        val address = bl.getAccount(dataToCheck.accountAddress, blNumber)
        val code = address.flatMap(a => bl.getEvmCodeByHash(a.codeHash))

        val storageCorrect = dataToCheck.accountStorage.forall { case (key, value) =>
          val stored = bl.getAccountStorageAt(address.get.storageRoot, key, ethCompatibleStorage = true)
          ByteUtils.toBigInt(stored) == value
        }

        if (address.isDefined && code.isDefined && storageCorrect) {
          go(remaining.tail)
        } else {
          false
        }
      }
    }

    go(nodeData)
  }
}
