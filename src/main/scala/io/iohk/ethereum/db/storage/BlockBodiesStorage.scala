package io.iohk.ethereum.db.storage

import java.nio.ByteBuffer

import akka.util.ByteString
import boopickle.Default.{ Pickle, Unpickle }
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockBodiesStorage.BlockBodyHash
import io.iohk.ethereum.domain.{ Address, BlockHeader, SignedTransaction, Transaction }
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.ByteUtils.compactPickledBytes

/**
  * This class is used to store the BlockBody, by using:
  *   Key: hash of the block to which the BlockBody belong
  *   Value: the block body
  */
class BlockBodiesStorage(val dataSource: DataSource) extends KeyValueStorage[BlockBodyHash, BlockBody, BlockBodiesStorage] {
  import BlockBodiesStorage._

  override val namespace: IndexedSeq[Byte] = Namespaces.BodyNamespace


  override def keySerializer: BlockBodyHash => IndexedSeq[Byte] = identity

  override def valueSerializer: BlockBody => IndexedSeq[Byte] = tl => compactPickledBytes(Pickle.intoBytes(tl))

  override def valueDeserializer: IndexedSeq[Byte] => BlockBody =
    b => Unpickle[BlockBodiesStorage].fromBytes(ByteBuffer.wrap(b.toArray[Byte]))
    //BlockBodiesStorage.fromBytes(b.toArray[Byte])

  override protected def apply(dataSource: DataSource): BlockBodiesStorage = new BlockBodiesStorage(dataSource)
}

object BlockBodiesStorage {
  type BlockBodyHash = ByteString

  import boopickle.DefaultBasic._

  implicit val byteStringPickler: Pickler[BlockBodyHash] = transformPickler[BlockBodyHash, Array[Byte]](ByteString(_))(_.toArray[Byte])
  implicit val addressPickler: Pickler[Address] = generatePickler[Address]
  implicit val transactionPickler: Pickler[Transaction] = generatePickler[Transaction]
  implicit val ecdsaSignaturePickler: Pickler[ECDSASignature] = generatePickler[ECDSASignature]
  implicit val signedTransactionPickler: Pickler[SignedTransaction] = transformPickler[SignedTransaction, (Transaction, ECDSASignature)]
  { case (tx, signature) => new SignedTransaction(tx, signature) }{ t => (t.tx, t.signature)}

  implicit val blockHeaderPickler: Pickler[BlockHeader] = generatePickler[BlockHeader]
  implicit val blockBodyPickler: Pickler[BlockBody] = transformPickler[BlockBody, (Seq[SignedTransaction], Seq[BlockHeader])]
  {case (stx, nodes) => new BlockBody(stx, nodes) }{ b => (b.transactionList, b.uncleNodesList) }

//  import io.iohk.ethereum.rlp.RLPImplicitConversions._
//  import io.iohk.ethereum.rlp.RLPImplicits._
//
//  private d signedTransactionToBytes(signedTx: SignedTransaction): RLPEncodeable = {
//    import signedTx._
//    import signedTx.tx._
//    RLPList(nonce, gasPrice, gasLimit, receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray): Array[Byte], value,
//      payload, signature.v, signature.r, signature.s)
//  }
//
//  private def signedTransactionFromEncodable(rlpEncodeable: RLPEncodeable): SignedTransaction = rlpEncodeable match {
//    case RLPList(nonce, gasPrice, gasLimit, (receivingAddress: RLPValue), value,
//    payload, pointSign, signatureRandom, signature) =>
//      val receivingAddressOpt = if(receivingAddress.bytes.isEmpty) None else Some(Address(receivingAddress.bytes))
//      SignedTransaction(
//        Transaction(nonce, gasPrice, gasLimit, receivingAddressOpt, value, payload),
//        (pointSign: Int).toByte,
//        signatureRandom,
//        signature
//      )
//  }


//  private[BlockBodiesStorage] def toBytes(blockBody: BlockBody): IndexedSeq[Byte] = {
//    encode(BlockBody.blockBodyToRlpEncodable(
//      blockBody,
//      signedTransactionToBytes,
//      header => BlockHeaderEnc(header).toRLPEncodable
//    ))
//  }

//  private[BlockBodiesStorage] def fromBytes(bytes: Array[Byte]): BlockBody = {
//    BlockBody.rlpEncodableToBlockBody(
//      rawDecode(bytes),
//      signedTransactionFromEncodable,
//      rlp => BlockheaderEncodableDec(rlp).toBlockHeader
//    )
//  }
}
