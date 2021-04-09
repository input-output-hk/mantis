package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class AppStateStorageSpec extends AnyWordSpec with ScalaCheckPropertyChecks with ObjectGenerators {

  "AppStateStorage" should {

    "insert and get best block number properly" in new Fixtures {
      forAll(ObjectGenerators.bigIntGen) { bestBlockNumber =>
        val storage = newAppStateStorage()
        storage.putBestBlockNumber(bestBlockNumber).commit()

        assert(storage.getBestBlockNumber() == bestBlockNumber)
      }
    }

    "insert and get best block hash properly" in new Fixtures {
      forAll(ObjectGenerators.byteStringOfLengthNGen(20)) { bestBlockHash =>
        val storage = newAppStateStorage()
        storage.putBestBlockHash(bestBlockHash).commit()

        assert(storage.getBestBlockHash().get == bestBlockHash)
      }
    }

    "get zero as best block number when storage is empty" in new Fixtures {
      assert(newAppStateStorage().getBestBlockNumber() == 0)
    }

    "insert and get fast sync done properly" in new Fixtures {
      val storage = newAppStateStorage()
      storage.fastSyncDone().commit()

      assert(storage.isFastSyncDone())
    }

    "get fast sync done false when storage is empty" in new Fixtures {
      assert(!newAppStateStorage().isFastSyncDone())
    }

    "insert and get estimated highest block properly" in new Fixtures {
      forAll(ObjectGenerators.bigIntGen) { estimatedHighestBlock =>
        val storage = newAppStateStorage()
        storage.putEstimatedHighestBlock(estimatedHighestBlock).commit()

        assert(storage.getEstimatedHighestBlock() == estimatedHighestBlock)
      }
    }

    "get zero as estimated highest block when storage is empty" in new Fixtures {
      assert(newAppStateStorage().getEstimatedHighestBlock() == 0)
    }

    "insert and get sync starting block properly" in new Fixtures {
      forAll(ObjectGenerators.bigIntGen) { syncStartingBlock =>
        val storage = newAppStateStorage()
        storage.putSyncStartingBlock(syncStartingBlock).commit()

        assert(storage.getSyncStartingBlock() == syncStartingBlock)
      }
    }

    "get zero as sync starting block when storage is empty" in new Fixtures {
      assert(newAppStateStorage().getSyncStartingBlock() == 0)
    }

    "update and remove latest checkpoint block number properly" in new Fixtures {
      forAll(ObjectGenerators.bigIntGen) { latestCheckpointBlockNumber =>
        val storage = newAppStateStorage()

        storage.putLatestCheckpointBlockNumber(latestCheckpointBlockNumber).commit()
        assert(storage.getLatestCheckpointBlockNumber() == latestCheckpointBlockNumber)

        storage.removeLatestCheckpointBlockNumber().commit()
        assert(storage.getLatestCheckpointBlockNumber() == 0)
      }
    }

    "update checkpoint block number and get it properly" in new Fixtures {
      forAll(ObjectGenerators.bigIntGen) { latestCheckpointBlockNumber =>
        val storage = newAppStateStorage()
        storage.putLatestCheckpointBlockNumber(latestCheckpointBlockNumber).commit()

        assert(storage.getLatestCheckpointBlockNumber() == latestCheckpointBlockNumber)
      }
    }

    "get zero as checkpoint block number when storage is empty" in new Fixtures {
      assert(newAppStateStorage().getBestBlockNumber() == 0)
    }
  }

  trait Fixtures {
    def newAppStateStorage(): AppStateStorage = new AppStateStorage(EphemDataSource())
  }

}
