package io.iohk.ethereum.db

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.mpt.{DataSource, EphemDataSource}
import io.iohk.ethereum.network.p2p.messages.PV63.Receipt
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

import scala.util.Random

class ReceiptStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators{
  test("ReceiptStorage insert") {
    forAll(Gen.listOf(anyArrayGen)){ blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val receipts = receiptsGen(blockHashes.length).sample.get
      val blockHashesReceiptsPair = receipts.zip(blockHashes)

      val dataSource = blockHashesReceiptsPair.foldLeft(EphemDataSource(): DataSource){
        case (recDataSource, (receiptList, blockHash)) =>
          ReceiptStorage.put(blockHash, receiptList, recDataSource)
      }

      blockHashesReceiptsPair.foreach{case (rs, bh) =>
        val obtainedReceipts: Option[Seq[Receipt]] = ReceiptStorage.get(bh, dataSource)
        assert(obtainedReceipts.isDefined && (obtainedReceipts.get equals rs))
      }
    }
  }

  test("ReceiptStorage delete") {
    forAll(Gen.listOf(anyArrayGen)){ blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val receipts = receiptsGen(blockHashes.length).sample.get
      val blockHashesReceiptsPair = receipts.zip(blockHashes)

      //Receipts are inserted
      val dataSource = blockHashesReceiptsPair.foldLeft(EphemDataSource(): DataSource){
        case (recDataSource, (receiptList, blockHash)) =>
          ReceiptStorage.put(blockHash, receiptList, recDataSource)
      }

      //Receipts are deleted
      val (toDelete, toLeave) = Random.shuffle(blockHashesReceiptsPair).splitAt(Gen.choose(0, blockHashesReceiptsPair.size).sample.get)
      val dataSourceAfterDelete = toDelete.foldLeft(dataSource: DataSource){
        case (recDataSource, (_, blockHash)) =>
          ReceiptStorage.remove(blockHash, recDataSource)
      }

      toLeave.foreach{case (rs, bh) =>
        val obtainedReceipts = ReceiptStorage.get(bh, dataSourceAfterDelete)
        assert(obtainedReceipts.isDefined && (obtainedReceipts.get equals rs))
      }
      toDelete.foreach{ case (_, bh) => assert(ReceiptStorage.get(bh, dataSourceAfterDelete).isEmpty) }
    }
  }
}
