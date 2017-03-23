package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorSystem
import akka.pattern._
import akka.testkit.TestActorRef
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.sync.FastSync.SyncState
import io.iohk.ethereum.blockchain.sync.FastSyncStateActor.GetStorage
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.FastSyncStateStorage
import io.iohk.ethereum.domain.BlockHeader
import org.scalatest.concurrent.Eventually
import org.scalatest.time._
import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.concurrent.duration._

class FastSyncStateActorSpec extends AsyncFlatSpec with Matchers with Eventually {

  "FastSyncStateActor" should "eventually persist a newest state of a fast sync" in {

    val dataSource = EphemDataSource()
    implicit val system = ActorSystem("FastSyncStateActorSpec_System")
    implicit val timeout: Timeout = 20.seconds
    val syncStateActor = TestActorRef(new FastSyncStateActor)
    val maxN = 10
    implicit val patienceConfig = PatienceConfig(timeout = Span(3, Seconds))

    val targetBlockHeader = BlockHeader(ByteString(""), ByteString(""), ByteString(""), ByteString(""), ByteString(""),
      ByteString(""), ByteString(""), 0, 0, 0, 0, 0, ByteString(""), ByteString(""), ByteString(""))
    syncStateActor ! new FastSyncStateStorage(dataSource)
    (0 to maxN).foreach(n => syncStateActor ! SyncState(targetBlockHeader).copy(downloadedNodesCount = n))

    eventually {
      (syncStateActor ? GetStorage).mapTo[Option[SyncState]].map { syncState =>
        val expected = SyncState(targetBlockHeader).copy(downloadedNodesCount = maxN)
        syncState shouldEqual Some(expected)
      }
    }

  }

}
