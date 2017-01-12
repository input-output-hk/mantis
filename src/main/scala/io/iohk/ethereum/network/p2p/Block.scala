package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.rlp.RLPImplicits._
import org.spongycastle.util.encoders.Hex

object BlockBody {
  implicit val rlpEndDec = new RLPEncoder[BlockBody] with RLPDecoder[BlockBody] {
    override def encode(obj: BlockBody): RLPEncodeable = {
      import obj._
      RLPList(
        RLPList(transactionList.map(Transaction.rlpEndDec.encode): _*),
        RLPList(uncleNodesList.map(BlockHeader.rlpEndDec.encode): _*))
    }

    override def decode(rlp: RLPEncodeable): BlockBody = rlp match {
      case RLPList((transactions: RLPList), (uncles: RLPList)) =>
        BlockBody(
          transactions.items.map(Transaction.rlpEndDec.decode),
          uncles.items.map(BlockHeader.rlpEndDec.decode))
      case _ => throw new RuntimeException("Cannot decode BlockBody")
    }
  }
}

case class BlockBody(transactionList: Seq[Transaction], uncleNodesList: Seq[BlockHeader]) {
  override def toString: String =
    s"""BlockBody{
       |transactionList: $transactionList
       |uncleNodesList: $uncleNodesList
       |}
    """.stripMargin
}

object BlockHeader {
  implicit val rlpEndDec = new RLPEncoder[BlockHeader] with RLPDecoder[BlockHeader] {
    override def encode(obj: BlockHeader): RLPEncodeable = {
      import obj._
      RLPList(
        parentHash.toArray[Byte],
        ommersHash.toArray[Byte],
        beneficiary.toArray[Byte],
        stateRoot.toArray[Byte],
        transactionsRoot.toArray[Byte],
        receiptsRoot.toArray[Byte],
        logsBloom.toArray[Byte],
        difficulty,
        number,
        gasLimit,
        gasUsed,
        unixTimestamp,
        extraData.toArray[Byte],
        mixHash.toArray[Byte],
        nonce.toArray[Byte])
    }

    override def decode(rlp: RLPEncodeable): BlockHeader = rlp match {
      case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot, logsBloom,
      difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce) =>
        BlockHeader(ByteString(parentHash: Array[Byte]),
          ByteString(ommersHash: Array[Byte]),
          ByteString(beneficiary: Array[Byte]),
          ByteString(stateRoot: Array[Byte]),
          ByteString(transactionsRoot: Array[Byte]),
          ByteString(receiptsRoot: Array[Byte]),
          ByteString(logsBloom: Array[Byte]),
          difficulty,
          number,
          gasLimit,
          gasUsed,
          unixTimestamp,
          ByteString(extraData: Array[Byte]),
          ByteString(mixHash: Array[Byte]),
          ByteString(nonce: Array[Byte]))

      case _ => throw new RuntimeException("Cannot decode BlockHeaders")
    }
  }
}

case class BlockHeader(
                        parentHash: ByteString,
                        ommersHash: ByteString,
                        beneficiary: ByteString,
                        stateRoot: ByteString,
                        transactionsRoot: ByteString,
                        receiptsRoot: ByteString,
                        logsBloom: ByteString,
                        difficulty: BigInt,
                        number: BigInt,
                        gasLimit: BigInt,
                        gasUsed: BigInt,
                        unixTimestamp: Long,
                        extraData: ByteString,
                        mixHash: ByteString,
                        nonce: ByteString) {

  override def toString: String = {
    s"""BlockHeader {
       |parentHash: ${Hex.toHexString(parentHash.toArray[Byte])}
       |ommersHash: ${Hex.toHexString(ommersHash.toArray[Byte])}
       |beneficiary: ${Hex.toHexString(beneficiary.toArray[Byte])}
       |stateRoot: ${Hex.toHexString(stateRoot.toArray[Byte])}
       |transactionsRoot: ${Hex.toHexString(transactionsRoot.toArray[Byte])}
       |receiptsRoot: ${Hex.toHexString(receiptsRoot.toArray[Byte])}
       |logsBloom: ${Hex.toHexString(logsBloom.toArray[Byte])}
       |difficulty: $difficulty,
       |number: $number,
       |gasLimit: $gasLimit,
       |gasUsed: $gasUsed,
       |unixTimestamp: $unixTimestamp,
       |extraData: ${Hex.toHexString(extraData.toArray[Byte])}
       |mixHash: ${Hex.toHexString(mixHash.toArray[Byte])}
       |nonce: ${Hex.toHexString(nonce.toArray[Byte])}
       |}""".stripMargin
  }
}
