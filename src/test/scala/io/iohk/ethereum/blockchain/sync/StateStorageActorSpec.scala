package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorSystem
import akka.pattern._
import akka.testkit.TestActorRef
import akka.testkit.TestKit

import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.blockchain.sync.fast.FastSync.SyncState
import io.iohk.ethereum.blockchain.sync.fast.StateStorageActor
import io.iohk.ethereum.blockchain.sync.fast.StateStorageActor.GetStorage
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.FastSyncStateStorage

class StateStorageActorSpec
    extends TestKit(ActorSystem("FastSyncStateActorSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with Eventually
    with NormalPatience {

  "FastSyncStateActor" should "eventually persist a newest state of a fast sync" in {
    val dataSource = EphemDataSource()
    val syncStateActor = TestActorRef(new StateStorageActor)
    val maxN = 10

    val targetBlockHeader = Fixtures.Blocks.ValidBlock.header
    syncStateActor ! new FastSyncStateStorage(dataSource)
    (0 to maxN).foreach(n => syncStateActor ! SyncState(targetBlockHeader).copy(downloadedNodesCount = n))

    eventually {
      (syncStateActor ? GetStorage)
        .mapTo[Option[SyncState]]
        .map { syncState =>
          val expected = SyncState(targetBlockHeader).copy(downloadedNodesCount = maxN)
          syncState shouldEqual Some(expected)
        }(system.dispatcher)
    }
  }
}
