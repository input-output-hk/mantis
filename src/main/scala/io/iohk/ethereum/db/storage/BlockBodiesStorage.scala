package io.iohk.ethereum.db.storage

import java.nio.ByteBuffer

import akka.util.ByteString
import boopickle.Default.{ Pickle, Unpickle }
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockBodiesStorage.BlockBodyHash
import io.iohk.ethereum.domain.{ Address, BlockHeader, BlockBody, SignedTransaction, Transaction }
import io.iohk.ethereum.utils.ByteUtils.compactPickledBytes

/**
  * This class is used to store the BlockBody, by using:
  *   Key: hash of the block to which the BlockBody belong
  *   Value: the block body
  */
class BlockBodiesStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[BlockBodyHash, BlockBody] {
  import BlockBodiesStorage._

  override val namespace: IndexedSeq[Byte] = Namespaces.BodyNamespace

  override def keySerializer: BlockBodyHash => IndexedSeq[Byte] = _.toIndexedSeq

  override def valueSerializer: BlockBody => IndexedSeq[Byte] = blockBody => compactPickledBytes(Pickle.intoBytes(blockBody))

  override def valueDeserializer: IndexedSeq[Byte] => BlockBody =
    bytes => Unpickle[BlockBody].fromBytes(ByteBuffer.wrap(bytes.toArray[Byte]))
}

object BlockBodiesStorage {
  type BlockBodyHash = ByteString

  import boopickle.DefaultBasic._

  implicit val byteStringPickler: Pickler[ByteString] = transformPickler[ByteString, Array[Byte]](ByteString(_))(_.toArray[Byte])
  implicit val addressPickler: Pickler[Address] =
    transformPickler[Address, ByteString](bytes => Address(bytes))(address => address.bytes)
  implicit val transactionPickler: Pickler[Transaction] = generatePickler[Transaction]
  implicit val ecdsaSignaturePickler: Pickler[ECDSASignature] = generatePickler[ECDSASignature]
  implicit val signedTransactionPickler: Pickler[SignedTransaction] = transformPickler[SignedTransaction, (Transaction, ECDSASignature)]
  { case (tx, signature) => new SignedTransaction(tx, signature) }{ stx => (stx.tx, stx.signature)}

  implicit val blockHeaderPickler: Pickler[BlockHeader] = generatePickler[BlockHeader]
  implicit val blockBodyPickler: Pickler[BlockBody] = transformPickler[BlockBody, (Seq[SignedTransaction], Seq[BlockHeader])]
  {case (stx, nodes) => BlockBody(stx, nodes) }{ blockBody => (blockBody.transactionList, blockBody.uncleNodesList) }

}
