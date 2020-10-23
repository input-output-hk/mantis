package io.iohk.ethereum.sync.util

import akka.util.ByteString
import cats.effect.Resource
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.blockchain.sync.FastSync
import io.iohk.ethereum.blockchain.sync.FastSync.SyncState
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.mpt.{HashNode, MptNode, MptTraversals}
import io.iohk.ethereum.sync.util.SyncCommonItSpecUtils.FakePeerCustomConfig.defaultConfig
import io.iohk.ethereum.sync.util.SyncCommonItSpecUtils._
import io.iohk.ethereum.utils.ByteUtils
import monix.eval.Task

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Try
object FastSyncItSpecUtils {

  class FakePeer(peerName: String, fakePeerCustomConfig: FakePeerCustomConfig)
      extends CommonFakePeer(peerName, fakePeerCustomConfig) {

    lazy val validators = new MockValidatorsAlwaysSucceed

    lazy val fastSync = system.actorOf(
      FastSync.props(
        storagesInstance.storages.fastSyncStateStorage,
        storagesInstance.storages.appStateStorage,
        bl,
        validators,
        peerEventBus,
        etcPeerManager,
        testSyncConfig,
        system.scheduler
      )
    )

    def startFastSync(): Task[Unit] = Task {
      fastSync ! FastSync.Start
    }

    def waitForFastSyncFinish(): Task[Boolean] = {
      retryUntilWithDelay(Task(storagesInstance.storages.appStateStorage.isFastSyncDone()), 1.second, 90) { isDone =>
        isDone
      }
    }

    // Reads whole trie into memory, if the trie lacks nodes in storage it will be None
    def getBestBlockTrie(): Option[MptNode] = {
      Try {
        val bestBlock = bl.getBestBlock()
        val bestStateRoot = bestBlock.header.stateRoot
        MptTraversals.parseTrieIntoMemory(
          HashNode(bestStateRoot.toArray),
          storagesInstance.storages.stateStorage.getBackingStorage(bestBlock.number)
        )
      }.toOption
    }

    def containsExpectedDataUpToAccountAtBlock(n: BigInt, blockNumber: BigInt): Boolean = {
      @tailrec
      def go(i: BigInt): Boolean = {
        if (i >= n) {
          true
        } else {
          val expectedBalance = i
          val accountAddress = Address(i)
          val accountExpectedCode = ByteString(i.toByteArray)
          val codeHash = kec256(accountExpectedCode)
          val accountExpectedStorageAddresses = (i until i + 20).toList
          val account = bl.getAccount(accountAddress, blockNumber).get
          val code = bl.getEvmCodeByHash(codeHash).get
          val storedData = accountExpectedStorageAddresses.map { addr =>
            ByteUtils.toBigInt(bl.getAccountStorageAt(account.storageRoot, addr, ethCompatibleStorage = true))
          }
          val haveAllStoredData = accountExpectedStorageAddresses.zip(storedData).forall { case (address, value) =>
            address == value
          }

          val dataIsCorrect =
            account.balance.toBigInt == expectedBalance && code == accountExpectedCode && haveAllStoredData
          if (dataIsCorrect) {
            go(i + 1)
          } else {
            false
          }
        }
      }

      go(0)
    }

    def startWithState(): Task[Unit] = {
      Task {
        val currentBest = bl.getBestBlock().header
        val safeTarget = currentBest.number + syncConfig.fastSyncBlockValidationX
        val nextToValidate = currentBest.number + 1
        val syncState = SyncState(currentBest, safeTarget, Seq(), Seq(), 0, 0, currentBest.number, nextToValidate)
        storagesInstance.storages.fastSyncStateStorage.putSyncState(syncState)
      }.map(_ => ())
    }

  }

  object FakePeer {

    def startFakePeer(peerName: String, fakePeerCustomConfig: FakePeerCustomConfig): Task[FakePeer] = {
      for {
        peer <- Task(new FakePeer(peerName, fakePeerCustomConfig))
        _ <- peer.startPeer()
      } yield peer
    }

    def start1FakePeerRes(
        fakePeerCustomConfig: FakePeerCustomConfig = defaultConfig,
        name: String
    ): Resource[Task, FakePeer] = {
      Resource.make {
        startFakePeer(name, fakePeerCustomConfig)
      } { peer =>
        peer.shutdown()
      }
    }

    def start2FakePeersRes(
        fakePeerCustomConfig1: FakePeerCustomConfig = defaultConfig,
        fakePeerCustomConfig2: FakePeerCustomConfig = defaultConfig
    ): Resource[Task, (FakePeer, FakePeer)] = {
      for {
        peer1 <- start1FakePeerRes(fakePeerCustomConfig1, "Peer1")
        peer2 <- start1FakePeerRes(fakePeerCustomConfig2, "Peer2")
      } yield (peer1, peer2)
    }

    def start3FakePeersRes(
        fakePeerCustomConfig1: FakePeerCustomConfig = defaultConfig,
        fakePeerCustomConfig2: FakePeerCustomConfig = defaultConfig,
        fakePeerCustomConfig3: FakePeerCustomConfig = defaultConfig
    ): Resource[Task, (FakePeer, FakePeer, FakePeer)] = {
      for {
        peer1 <- start1FakePeerRes(fakePeerCustomConfig1, "Peer1")
        peer2 <- start1FakePeerRes(fakePeerCustomConfig2, "Peer2")
        peer3 <- start1FakePeerRes(fakePeerCustomConfig3, "Peer3")
      } yield (peer1, peer2, peer3)
    }
  }

}
