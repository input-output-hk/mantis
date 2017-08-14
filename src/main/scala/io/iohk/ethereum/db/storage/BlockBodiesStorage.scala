package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockBodiesStorage.BlockBodyHash
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._

/**
  * This class is used to store the BlockBody, by using:
  *   Key: hash of the block to which the BlockBody belong
  *   Value: the block body
  */
class BlockBodiesStorage(val dataSource: DataSource) extends KeyValueStorage[BlockBodyHash, BlockBody, BlockBodiesStorage] {

  override val namespace: IndexedSeq[Byte] = Namespaces.BodyNamespace

  override def keySerializer: (BlockBodyHash) => IndexedSeq[Byte] = identity

  override def valueSerializer: (BlockBody) => IndexedSeq[Byte] = BlockBodiesStorage.toBytes

  override def valueDeserializer: (IndexedSeq[Byte]) => BlockBody = b => BlockBodiesStorage.fromBytes(b.toArray[Byte])

  override protected def apply(dataSource: DataSource): BlockBodiesStorage = new BlockBodiesStorage(dataSource)
}

object BlockBodiesStorage {
  type BlockBodyHash = ByteString

  import io.iohk.ethereum.rlp.RLPImplicitConversions._
  import io.iohk.ethereum.rlp.RLPImplicits._

  private def signedTransactionToBytes(signedTx: SignedTransaction): RLPEncodeable = {
    import signedTx._
    import signedTx.tx._
    RLPList(nonce, gasPrice, gasLimit, receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray): Array[Byte], value,
      payload, signature.v, signature.r, signature.s, senderAddress.toArray)
  }

  private def signedTransactionFromEncodable(rlpEncodeable: RLPEncodeable): SignedTransaction = rlpEncodeable match {
    case RLPList(nonce, gasPrice, gasLimit, (receivingAddress: RLPValue), value,
    payload, pointSign, signatureRandom, signature, senderAddress) =>
      val receivingAddressOpt = if(receivingAddress.bytes.isEmpty) None else Some(Address(receivingAddress.bytes))
      SignedTransaction(
        Transaction(nonce, gasPrice, gasLimit, receivingAddressOpt, value, payload),
        (pointSign: Int).toByte,
        signatureRandom,
        signature,
        Address(senderAddress: Array[Byte])
      )
  }


  private[BlockBodiesStorage] def toBytes(blockBody: BlockBody): IndexedSeq[Byte] = {
    import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
    encode(BlockBody.blockBodyToRlpEncodable(
      blockBody,
      signedTransactionToBytes,
      header => BlockHeaderEnc(header).toRLPEncodable
    ))
  }

  private[BlockBodiesStorage] def fromBytes(bytes: Array[Byte]): BlockBody = {
    import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
    BlockBody.rlpEncodableToBlockBody(
      rawDecode(bytes),
      signedTransactionFromEncodable,
      rlp => BlockheaderEncodableDec(rlp).toBlockHeader
    )
  }
}
