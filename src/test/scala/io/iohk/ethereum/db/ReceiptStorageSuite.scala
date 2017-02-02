package io.iohk.ethereum.db

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.network.p2p.messages.PV63.Receipt
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

import scala.util.Random

class ReceiptStorageSuite extends FunSuite with PropertyChecks with ObjectGenerators {

  test("ReceiptStorage insert") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))){ blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val receipts = receiptsGen(blockHashes.length).sample.get
      val blockHashesReceiptsPair = receipts.zip(blockHashes)

      val initialReceiptStorage = new ReceiptStorage(EphemDataSource())
      val receiptStorage = blockHashesReceiptsPair.foldLeft(initialReceiptStorage){
        case (recReceiptStorage, (receiptList, blockHash)) =>
          recReceiptStorage.put(blockHash, receiptList)
      }

      blockHashesReceiptsPair.foreach{case (rs, bh) =>
        val obtainedReceipts: Option[Seq[Receipt]] = receiptStorage.get(bh)
        assert(obtainedReceipts.contains(rs))
      }
    }
  }

  test("ReceiptStorage delete") {
    forAll(Gen.listOf(byteStringOfLengthNGen(32))){ blockByteArrayHashes =>
      val blockHashes = blockByteArrayHashes.distinct
      val receipts = receiptsGen(blockHashes.length).sample.get
      val blockHashesReceiptsPair = receipts.zip(blockHashes)

      //Receipts are inserted
      val initialReceiptStorage = new ReceiptStorage(EphemDataSource())
      val receiptStorage = blockHashesReceiptsPair.foldLeft(initialReceiptStorage){
        case (recReceiptStorage, (receiptList, blockHash)) =>
          recReceiptStorage.put(blockHash, receiptList)
      }

      //Receipts are deleted
      val (toDelete, toLeave) = Random.shuffle(blockHashesReceiptsPair)
        .splitAt(Gen.choose(0, blockHashesReceiptsPair.size).sample.get)
      val receiptStorageAfterDelete = toDelete.foldLeft(receiptStorage){
        case (recReceiptStorage, (_, blockHash)) =>
          recReceiptStorage.remove(blockHash)
      }

      toLeave.foreach{case (rs, bh) =>
        val obtainedReceipts = receiptStorageAfterDelete.get(bh)
        assert(obtainedReceipts.contains(rs))
      }
      toDelete.foreach{ case (_, bh) => assert(receiptStorageAfterDelete.get(bh).isEmpty) }
    }
  }
}