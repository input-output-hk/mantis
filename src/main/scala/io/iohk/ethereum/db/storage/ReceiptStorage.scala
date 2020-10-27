package io.iohk.ethereum.db.storage

import java.nio.ByteBuffer

import akka.util.ByteString
import boopickle.Default.{Pickle, Unpickle}
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.ReceiptStorage._
import io.iohk.ethereum.domain.{Address, SuccessOutcome, _}
import io.iohk.ethereum.utils.ByteUtils.compactPickledBytes
import boopickle.DefaultBasic._

/**
  * This class is used to store the Receipts, by using:
  * Key: hash of the block to which the list of receipts belong
  * Value: the list of receipts
  */
class ReceiptStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[BlockHash, Seq[Receipt]] {

  import ReceiptStorage._

  override val namespace: IndexedSeq[Byte] = Namespaces.ReceiptsNamespace

  override def keySerializer: BlockHash => IndexedSeq[Byte] = _.toIndexedSeq

  override def keyDeserializer: IndexedSeq[Byte] => BlockHash = k => ByteString.fromArrayUnsafe(k.toArray)

  override def valueSerializer: ReceiptSeq => IndexedSeq[Byte] = receipts =>
    compactPickledBytes(Pickle.intoBytes(receipts))

  override def valueDeserializer: IndexedSeq[Byte] => ReceiptSeq =
    bytes => Unpickle[Seq[Receipt]].fromBytes(ByteBuffer.wrap(bytes.toArray[Byte]))
}

object ReceiptStorage {
  type BlockHash = ByteString
  type ReceiptSeq = Seq[Receipt]

  implicit val byteStringPickler: Pickler[ByteString] =
    transformPickler[ByteString, Array[Byte]](ByteString(_))(_.toArray[Byte])
  implicit val hashOutcomePickler: Pickler[HashOutcome] = transformPickler[HashOutcome, ByteString] { hash =>
    HashOutcome(hash)
  } { outcome => outcome.stateHash }
  implicit val successOutcomePickler: Pickler[SuccessOutcome.type] = transformPickler[SuccessOutcome.type, ByteString] {
    _ => SuccessOutcome
  } { _ => ByteString(Array(1.toByte)) }
  implicit val failureOutcomePickler: Pickler[FailureOutcome.type] = transformPickler[FailureOutcome.type, ByteString] {
    _ => FailureOutcome
  } { _ => ByteString(Array(0.toByte)) }
  implicit val transactionOutcomePickler: Pickler[TransactionOutcome] = compositePickler[TransactionOutcome]
    .addConcreteType[HashOutcome]
    .addConcreteType[SuccessOutcome.type]
    .addConcreteType[FailureOutcome.type]

  implicit val addressPickler: Pickler[Address] =
    transformPickler[Address, ByteString](bytes => Address(bytes))(address => address.bytes)
  implicit val txLogEntryPickler: Pickler[TxLogEntry] =
    transformPickler[TxLogEntry, (Address, Seq[ByteString], ByteString)] { case (address, topics, data) =>
      TxLogEntry(address, topics, data)
    } { entry => (entry.loggerAddress, entry.logTopics, entry.data) }

  implicit val receiptPickler: Pickler[Receipt] =
    transformPickler[Receipt, (TransactionOutcome, BigInt, ByteString, Seq[TxLogEntry])] {
      case (state, gas, filter, logs) => new Receipt(state, gas, filter, logs)
    } { receipt =>
      (receipt.postTransactionStateHash, receipt.cumulativeGasUsed, receipt.logsBloomFilter, receipt.logs)
    }

}
