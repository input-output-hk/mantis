package io.iohk.ethereum.db.storage

import java.nio.ByteBuffer

import akka.util.ByteString
import boopickle.Default.{ Pickle, Unpickle }
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockHeadersStorage.BlockHeaderHash
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.ByteUtils.compactPickledBytes

/**
  * This class is used to store the BlockHeader, by using:
  *   Key: hash of the block to which the BlockHeader belong
  *   Value: the block header
  */
class BlockHeadersStorage(val dataSource: DataSource) extends KeyValueStorage[BlockHeaderHash, BlockHeader, BlockHeadersStorage] {

  import BlockHeadersStorage._

  override val namespace: IndexedSeq[Byte] = Namespaces.HeaderNamespace

  override def keySerializer: BlockHeaderHash => IndexedSeq[Byte] = _.toIndexedSeq

  override def valueSerializer: BlockHeader => IndexedSeq[Byte] =
    blockHeader => compactPickledBytes(Pickle.intoBytes(blockHeader))

  override def valueDeserializer: IndexedSeq[Byte] => BlockHeader =
    bytes => Unpickle[BlockHeader].fromBytes(ByteBuffer.wrap(bytes.toArray[Byte]))

  override protected def apply(dataSource: DataSource): BlockHeadersStorage = new BlockHeadersStorage(dataSource)

}

object BlockHeadersStorage {
  type BlockHeaderHash = ByteString
  /** The following types are [[io.iohk.ethereum.domain.BlockHeader]] param types (in exact order).
    *
    * Mentioned params:
    * parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot, logsBloom,
    * difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce.
    */
  type BlockHeaderBody = (
    ByteString,
    ByteString,
    ByteString,
    ByteString,
    ByteString,
    ByteString,
    ByteString,
    BigInt,
    BigInt,
    BigInt,
    BigInt,
    Long,
    ByteString,
    ByteString,
    ByteString,
    Option[Boolean]
  )

  import boopickle.DefaultBasic._

  implicit val byteStringPickler: Pickler[ByteString] = transformPickler[ByteString, Array[Byte]](ByteString(_))(_.toArray[Byte])
  implicit val blockHeaderPickler: Pickler[BlockHeader] = transformPickler[BlockHeader, BlockHeaderBody]
  { case (ph, oh, b, sr, txr, rr, lb, d, no, gl, gu, ut, ed, mh, n, oo) =>
    new BlockHeader(ph, oh, b, sr, txr, rr, lb, d, no, gl, gu, ut, ed, mh, n, oo)
  }{ blockHeader => (
    blockHeader.parentHash,
    blockHeader.ommersHash,
    blockHeader.beneficiary,
    blockHeader.stateRoot,
    blockHeader.transactionsRoot,
    blockHeader.receiptsRoot,
    blockHeader.logsBloom,
    blockHeader.difficulty,
    blockHeader.number,
    blockHeader.gasLimit,
    blockHeader.gasUsed,
    blockHeader.unixTimestamp,
    blockHeader.extraData,
    blockHeader.mixHash,
    blockHeader.nonce,
    blockHeader.treasuryOptOut
  )}
}
