package io.iohk.ethereum.sync.util

import akka.actor.ActorSystem
import cats.effect.Resource
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.blockchain.sync.FastSync
import io.iohk.ethereum.mpt.{HashNode, MptNode, MptTraversals}
import io.iohk.ethereum.sync.util.SyncUtils.retryUntilWithDelay
import monix.eval.Task

import scala.concurrent.duration._
import scala.util.Try

class FakePeerFastSync(peerName: String) extends FakePeer {

  override implicit protected def system: ActorSystem = ActorSystem(peerName)

  override def start(): Task[Unit] = Task {
    fastSync ! FastSync.Start
  }

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
}

object FakePeerFastSync {
  def startFakePeer(peerName: String): Task[FakePeerFastSync] = {
    for {
      peer <- Task(new FakePeerFastSync(peerName))
      _ <- peer.startPeer()
    } yield peer
  }

  def start2FakePeersRes(): Resource[Task, (FakePeerFastSync, FakePeerFastSync)] = {
    Resource.make {
      Task.parZip2(startFakePeer("Peer1"), startFakePeer("Peer2"))
    } { case (peer, peer1) => Task.parMap2(peer.shutdown(), peer1.shutdown())((_, _) => ()) }
  }

  def start3FakePeersRes(): Resource[Task, (FakePeerFastSync, FakePeerFastSync, FakePeerFastSync)] = {
    Resource.make {
      Task.parZip3(startFakePeer("Peer1"), startFakePeer("Peer2"), startFakePeer("Peer3"))
    } { case (peer, peer1, peer2) =>
      Task.parMap3(peer.shutdown(), peer1.shutdown(), peer2.shutdown())((_, _, _) => ())
    }
  }
}
