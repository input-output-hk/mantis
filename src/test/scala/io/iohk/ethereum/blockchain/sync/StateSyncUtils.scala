package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString

import io.iohk.ethereum.blockchain.sync.fast.SyncStateScheduler.SyncResponse
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteUtils

object StateSyncUtils extends EphemBlockchainTestSetup {

  final case class MptNodeData(
      accountAddress: Address,
      accountCode: Option[ByteString],
      accountStorage: Seq[(BigInt, BigInt)],
      accountBalance: Int
  )

  class TrieProvider(
      blockchain: Blockchain,
      blockchainReader: BlockchainReader,
      evmCodeStorage: EvmCodeStorage,
      blockchainConfig: BlockchainConfig
  ) {
    def getNodes(hashes: List[ByteString]): List[SyncResponse] =
      hashes.map { hash =>
        val maybeResult = blockchainReader.getMptNodeByHash(hash) match {
          case Some(value) => Some(ByteString(value.encode))
          case None        => evmCodeStorage.get(hash)
        }
        maybeResult match {
          case Some(result) => SyncResponse(hash, result)
          case None         => throw new RuntimeException("Missing expected data in storage")
        }
      }

    def buildWorld(accountData: Seq[MptNodeData], existingTree: Option[ByteString] = None): ByteString = {
      val init = InMemoryWorldStateProxy(
        evmCodeStorage,
        blockchain.getBackingMptStorage(1),
        (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
        blockchainConfig.accountStartNonce,
        existingTree.getOrElse(ByteString(MerklePatriciaTrie.EmptyRootHash)),
        noEmptyAccounts = true,
        ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
      )

      val modifiedWorld = accountData.foldLeft(init) { case (world, data) =>
        val storage = world.getStorage(data.accountAddress)
        val modifiedStorage = data.accountStorage.foldLeft(storage) { case (s, v) =>
          s.store(v._1, v._2)
        }
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
      val freshBlockchainMetadata = getNewBlockchainMetadata
      val blockchainReader = BlockchainReader(freshStorage.storages, freshBlockchainMetadata)
      new TrieProvider(
        BlockchainImpl(freshStorage.storages, blockchainReader, freshBlockchainMetadata),
        blockchainReader,
        freshStorage.storages.evmCodeStorage,
        blockchainConfig
      )
    }
  }

  def createNodeDataStartingFrom(initialNumber: Int, lastNumber: Int, storageOffset: Int): Seq[MptNodeData] =
    (initialNumber until lastNumber).map { i =>
      val address = Address(i)
      val codeBytes = ByteString(BigInt(i).toByteArray)
      val storage = (initialNumber until initialNumber + storageOffset).map(s => (BigInt(s), BigInt(s)))
      val balance = i
      MptNodeData(address, Some(codeBytes), storage, balance)
    }

  def checkAllDataExists(
      nodeData: List[MptNodeData],
      blockchain: Blockchain,
      blockchainReader: BlockchainReader,
      evmCodeStorage: EvmCodeStorage,
      blNumber: BigInt
  ): Boolean = {
    def go(remaining: List[MptNodeData]): Boolean =
      if (remaining.isEmpty) {
        true
      } else {
        val dataToCheck = remaining.head
        val address =
          blockchainReader.getAccount(blockchainReader.getBestBranch(), dataToCheck.accountAddress, blNumber)
        val code = address.flatMap(a => evmCodeStorage.get(a.codeHash))

        val storageCorrect = dataToCheck.accountStorage.forall { case (key, value) =>
          val stored = blockchain.getAccountStorageAt(address.get.storageRoot, key, ethCompatibleStorage = true)
          ByteUtils.toBigInt(stored) == value
        }

        if (address.isDefined && code.isDefined && storageCorrect) {
          go(remaining.tail)
        } else {
          false
        }
      }

    go(nodeData)
  }
}
