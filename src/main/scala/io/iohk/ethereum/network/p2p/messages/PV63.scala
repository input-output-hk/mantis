package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => rlpDecode, encodeToRlp => rlpEncode}
import io.iohk.ethereum.rlp._
import org.spongycastle.util.encoders.Hex

object PV63 {

  object GetNodeData {
    implicit val rlpEndDec = new RLPEncoder[GetNodeData] with RLPDecoder[GetNodeData] {
      override def encode(obj: GetNodeData): RLPEncodeable = {
        import obj._
        hashes: RLPList
      }

      override def decode(rlp: RLPEncodeable): GetNodeData = rlp match {
        case rlpList: RLPList => GetNodeData(rlpList.items.map(rlpDecode[ByteString]))
        case _ => throw new RuntimeException("Cannot decode GetNodeData")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x0d
  }

  case class GetNodeData(hashes: Seq[ByteString]) extends Message {
    override def code: Int = GetNodeData.code

    override def toString: String = {
      s"""GetNodeData{
         |hashes: ${hashes.map(e => Hex.toHexString(e.toArray[Byte]))}
         |}
       """.stripMargin
    }
  }

  object NodeData {
    implicit val rlpEndDec = new RLPEncoder[NodeData] with RLPDecoder[NodeData] {
      override def encode(obj: NodeData): RLPEncodeable = {
        import obj._
        RLPList(values: _*)
      }

      override def decode(rlp: RLPEncodeable): NodeData = rlp match {
        case rlpList: RLPList => NodeData(rlpList.items)
        case _ => throw new RuntimeException("Cannot decode NodeData")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x0e
  }

  case class NodeData(values: Seq[RLPEncodeable]) extends Message {
    override def code: Int = NodeData.code
  }

  object GetReceipts {
    implicit val rlpEndDec = new RLPEncoder[GetReceipts] with RLPDecoder[GetReceipts] {
      override def encode(obj: GetReceipts): RLPEncodeable = {
        import obj._
        blockHashes: RLPList
      }

      override def decode(rlp: RLPEncodeable): GetReceipts = rlp match {
        case rlpList: RLPList => GetReceipts(rlpList.items.map(rlpDecode[ByteString]))
        case _ => throw new RuntimeException("Cannot decode GetReceipts")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x0f
  }

  case class GetReceipts(blockHashes: Seq[ByteString]) extends Message {
    override def code: Int = GetReceipts.code

    override def toString: String = {
      s"""GetReceipts{
         |blockHashes: ${blockHashes.map(e => Hex.toHexString(e.toArray[Byte]))}
         |}
       """.stripMargin
    }
  }

  object TransactionLog {
    implicit val rlpEndDec = new RLPEncoder[TransactionLog] with RLPDecoder[TransactionLog] {
      override def encode(obj: TransactionLog): RLPEncodeable = {
        import obj._
        RLPList(rlpEncode[ByteString](loggerAddress), RLPList(logTopics.map(rlpEncode[ByteString]): _*), rlpEncode[ByteString](data))
      }

      override def decode(rlp: RLPEncodeable): TransactionLog = rlp match {
        case RLPList(loggerAddress, logTopics: RLPList, data) =>
          TransactionLog(rlpDecode[ByteString](loggerAddress), logTopics.items.map(rlpDecode[ByteString]), rlpDecode[ByteString](data))
        case _ => throw new RuntimeException("Cannot decode TransactionLog")
      }
    }
  }

  case class TransactionLog(loggerAddress: ByteString, logTopics: Seq[ByteString], data: ByteString) {
    override def toString: String = {
      s"""TransactionLog{
         |loggerAddress: ${Hex.toHexString(loggerAddress.toArray[Byte])}
         |logTopics: ${logTopics.map(e => Hex.toHexString(e.toArray[Byte]))}
         |data: ${Hex.toHexString(data.toArray[Byte])}
         |}
       """.stripMargin
    }
  }

  object Receipt {
    implicit val rlpEndDec = new RLPEncoder[Receipt] with RLPDecoder[Receipt] {
      override def encode(obj: Receipt): RLPEncodeable = {
        import obj._
        RLPList(rlpEncode[ByteString](postTransactionStateHash), cumulativeGasUsed,
          rlpEncode[ByteString](logsBloomFilter), RLPList(logs.map(rlpEncode[TransactionLog]): _*))
      }

      override def decode(rlp: RLPEncodeable): Receipt = rlp match {
        case RLPList(postTransactionStateHash, cumulativeGasUsed, logsBloomFilter, logs: RLPList) =>
          Receipt(rlpDecode[ByteString](postTransactionStateHash), cumulativeGasUsed,
            rlpDecode[ByteString](logsBloomFilter), logs.items.map(rlpDecode[TransactionLog]))
        case _ => throw new RuntimeException("Cannot decode Receipt")
      }
    }
  }

  case class Receipt(
    postTransactionStateHash: ByteString,
    cumulativeGasUsed: BigInt,
    logsBloomFilter: ByteString,
    logs: Seq[TransactionLog]
  ) {
    override def toString: String = {
      s"""
         |Receipt{
         |postTransactionStateHash: ${Hex.toHexString(postTransactionStateHash.toArray[Byte])}
         |cumulativeGasUsed: $cumulativeGasUsed
         |logsBloomFilter: ${Hex.toHexString(logsBloomFilter.toArray[Byte])}
         |logs: $logs
         |}
       """.stripMargin
    }
  }

  object Receipts {
    implicit val rlpEndDec = new RLPEncoder[Receipts] with RLPDecoder[Receipts] {
      override def encode(obj: Receipts): RLPEncodeable = {
        import obj._
        RLPList(receiptsForBlocks.map(r => RLPList(r.map{rlpEncode[Receipt]}: _*)): _*)
      }

      override def decode(rlp: RLPEncodeable): Receipts = rlp match {
        case rlpList: RLPList => Receipts(rlpList.items.collect { case r: RLPList => r.items.map(rlpDecode[Receipt]) })
        case _ => throw new RuntimeException("Cannot decode Receipts")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x10
  }

  case class Receipts(receiptsForBlocks: Seq[Seq[Receipt]]) extends Message {
    override def code: Int = Receipts.code
  }
}
