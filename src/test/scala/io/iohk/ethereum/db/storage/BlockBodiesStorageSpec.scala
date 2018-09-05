package io.iohk.ethereum.db.storage

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import org.scalatest.WordSpec
import org.scalatest.prop.PropertyChecks


class BlockBodiesStorageSpec extends WordSpec with PropertyChecks with ObjectGenerators with SecureRandomBuilder {

  val chainId: Option[Byte] = Hex.decode("3d").headOption

  "BlockBodiesStorage" should {

    "insert block body properly" in {
      forAll(Gen.listOfN(32, ObjectGenerators.newBlockGen(secureRandom, chainId))) { newBlocks =>

        val initialStorage = new BlockBodiesStorage(EphemDataSource())
        val blocks = newBlocks.distinct
        val totalStorage = newBlocks.foldLeft(initialStorage){
          case (storage, NewBlock(block, _)) => storage.put(block.header.hash, block.body)
        }

        blocks.foreach{
          case NewBlock(block, _) => assert(totalStorage.get(block.header.hash).contains(block.body))
        }
      }
    }

    "delete block body properly" in {
      forAll(Gen.listOfN(32, ObjectGenerators.newBlockGen(secureRandom, chainId))) { newBlocks =>

        val initialStorage = new BlockBodiesStorage(EphemDataSource())
        val blocks = newBlocks.distinct
        val totalStorage = newBlocks.foldLeft(initialStorage){
          case (storage, NewBlock(block, _)) => storage.put(block.header.hash, block.body)
        }

        // Mapping of block bodies is inserted
        blocks.foreach{ case NewBlock(block, _) => assert(totalStorage.get(block.header.hash).contains(block.body)) }

        // Mapping of block bodies is deleted
        val (toDelete, toLeave) = blocks.splitAt(Gen.choose(0, blocks.size).sample.get)

        val storageAfterDelete = toDelete.foldLeft(totalStorage){
          case (storage, NewBlock(block, _)) => storage.remove(block.header.hash)
        }

        toLeave.foreach { case NewBlock(block, _) => assert(storageAfterDelete.get(block.header.hash).contains(block.body)) }
        toDelete.foreach { case NewBlock(block, _) => assert(storageAfterDelete.get(block.header.hash).isEmpty) }

      }
    }
  }
}
