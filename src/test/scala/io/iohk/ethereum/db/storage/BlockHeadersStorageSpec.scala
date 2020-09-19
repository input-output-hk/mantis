package io.iohk.ethereum.db.storage

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.domain.BlockHeader
import org.scalacheck.Gen
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BlockHeadersStorageSpec extends WordSpec with ScalaCheckPropertyChecks with ObjectGenerators {

  "BlockHeadersStorage" should {

    "insert block header properly" in {
      forAll(Gen.listOfN(32, ObjectGenerators.blockHeaderGen)) { blockHeaders =>
        val initialStorage = new BlockHeadersStorage(EphemDataSource())
        val headers = blockHeaders.distinct
        val totalStorage = blockHeaders.foldLeft(initialStorage) { case (storage, blockHeader) =>
          storage.put(blockHeader.hash, blockHeader)
        }

        checkIfIsInStorage(headers, totalStorage)
      }
    }

    "delete block header properly" in {
      forAll(Gen.listOfN(32, ObjectGenerators.blockHeaderGen)) { blockHeaders =>
        val initialStorage = new BlockHeadersStorage(EphemDataSource())
        val headers = blockHeaders.distinct
        val totalStorage = blockHeaders.foldLeft(initialStorage) { case (storage, blockHeader) =>
          storage.put(blockHeader.hash, blockHeader)
        }

        // Mapping of block headers is inserted
        checkIfIsInStorage(headers, totalStorage)

        // Mapping of block headers is deleted
        val (toDelete, toLeave) = headers.splitAt(Gen.choose(0, headers.size).sample.get)

        val storageAfterDelete = toDelete.foldLeft(totalStorage) { case (storage, blockHeader) =>
          storage.remove(blockHeader.hash)
        }

        checkIfIsInStorage(toLeave, storageAfterDelete)
        toDelete.foreach(header => assert(storageAfterDelete.get(header.hash).isEmpty))
      }
    }
  }

  def checkIfIsInStorage(headers: List[BlockHeader], totalStorage: BlockHeadersStorage): Unit = {
    headers.foreach(header => assert(totalStorage.get(header.hash).contains(header)))

  }
}
