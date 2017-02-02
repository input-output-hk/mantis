package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.mpt.HexPrefix.{decode => hpDecode, encode => hpEncode}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => rlpDecode}
import io.iohk.ethereum.rlp._
import org.spongycastle.util.encoders.Hex

import scala.util.Try

object PV63 {

  object GetNodeData {
    implicit val rlpEndDec = new RLPEncoder[GetNodeData] with RLPDecoder[GetNodeData] {
      override def encode(obj: GetNodeData): RLPEncodeable = {
        import obj._
        commonMptHashes: RLPList
      }

      override def decode(rlp: RLPEncodeable): GetNodeData = rlp match {
        case rlpList: RLPList => GetNodeData(rlpList.items.map(rlpDecode[ByteString]))
        case _ => throw new RuntimeException("Cannot decode GetNodeData")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x0d
  }

  case class GetNodeData(commonMptHashes: Seq[ByteString]) extends Message {
    override def code: Int = GetNodeData.code

    override def toString: String = {
      s"""GetNodeData{
         |hashes: ${commonMptHashes.map(e => Hex.toHexString(e.toArray[Byte]))}
         |}
       """.stripMargin
    }
  }

  object Account {
    implicit val rlpEndDec = new RLPEncoder[Account] with RLPDecoder[Account] {
      override def encode(obj: Account): RLPEncodeable = {
        import obj._
        RLPList(nonce, balance, byteStringEncDec.encode(storageRoot), byteStringEncDec.encode(codeHash))
      }

      override def decode(rlp: RLPEncodeable): Account = rlp match {
        case RLPList(nonce, balance, storageRoot, codeHash) =>
          Account(nonce, balance, byteStringEncDec.decode(storageRoot), byteStringEncDec.decode(codeHash))
        case _ => throw new RuntimeException("Cannot decode Account")
      }
    }
  }

  case class Account(nonce: BigInt, balance: BigInt, storageRoot: ByteString, codeHash: ByteString) {
    override def toString: String = {
      s"""Account{
         |nonce: $nonce
         |balance: $balance wei
         |storageRoot: ${Hex.toHexString(storageRoot.toArray[Byte])}
         |codeHash: ${Hex.toHexString(codeHash.toArray[Byte])}
         |}
       """.stripMargin
    }
  }

  object MptNode {
    val BranchNodeChildLength = 16

    implicit val rlpEndDec = new RLPEncoder[MptNode] with RLPDecoder[MptNode] {
      override def encode(obj: MptNode): RLPEncodeable = {
        obj match {
          case n: MptLeaf =>
            import n._
            RLPList(RLPValue(hpEncode(keyNibbles.toArray[Byte], isLeaf = true)), value)
          case n: MptExtension =>
            import n._
            RLPList(RLPValue(hpEncode(keyNibbles.toArray[Byte], isLeaf = false)), child.fold(_.hash, _.value): RLPEncodeable)
          case n: MptBranch =>
            import n._
            RLPList(children.map(e => e.fold(_.hash, _.value)).map(e => byteStringEncDec.encode(e)) :+ (terminator: RLPEncodeable): _*)
        }
      }

      override def decode(rlp: RLPEncodeable): MptNode = rlp match {
        case rlpList: RLPList if rlpList.items.length == BranchNodeChildLength + 1 =>
          MptBranch(rlpList.items.take(BranchNodeChildLength).map { bytes =>
            val v = rlpDecode[ByteString](bytes)
            if (v.nonEmpty && v.length < 32) {
              Right(MptValue(v))
            } else {
              Left(MptHash(v))
            }
          }, byteStringEncDec.decode(rlpList.items(16)))
        case RLPList(hpEncoded, value) =>
          hpDecode(hpEncoded: Array[Byte]) match {
            case (decoded, true) =>
              MptLeaf(ByteString(decoded), rlpDecode[ByteString](value))
            case (decoded, false) =>
              val v = rlpDecode[ByteString](value)
              val child = if (v.nonEmpty && v.length < 32)
                Right(MptValue(v))
              else
                Left(MptHash(v))

              MptExtension(ByteString(decoded), child)
          }
        case _ =>
          throw new RuntimeException("Cannot decode NodeData")
      }
    }
  }

  sealed trait MptNode

  object NodeData {
    implicit val rlpEndDec = new RLPEncoder[NodeData] with RLPDecoder[NodeData] {
      override def encode(obj: NodeData): RLPEncodeable = {
        import obj._
        values
      }

      override def decode(rlp: RLPEncodeable): NodeData = rlp match {
        case rlpList: RLPList =>
          NodeData(rlpList.items.map { e =>
            ByteString(e: Array[Byte])
          })
        case _ => throw new RuntimeException("Cannot decode NodeData")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x0e
  }

  case class NodeData(values: Seq[ByteString]) extends Message {
    override def code: Int = NodeData.code

    @throws[RLPException]
    def getMptNode(idx: Int): MptNode = rlpDecode[MptNode](values(idx).toArray[Byte])

    override def toString: String = {
      s"""NodeData{
         |values: ${values.map(b => Hex.toHexString(b.toArray[Byte]))}
         |}
       """.stripMargin
    }
  }

  case class MptBranch(children: Seq[Either[MptHash, MptValue]], terminator: ByteString) extends MptNode {
    require(children.length == 16, "MptBranch childHashes length have to be 16")

    override def toString: String = {
      val childrenString = children.map { e =>
        e.fold(a => s"Hash(${Hex.toHexString(a.hash.toArray[Byte])})", b => s"Value(${Hex.toHexString(b.value.toArray[Byte])})")
      }.mkString("(", ",\n", ")")

      s"""MptBranch{
         |children: $childrenString
         |terminator: ${Hex.toHexString(terminator.toArray[Byte])}
         |}
       """.stripMargin
    }
  }

  case class MptExtension(keyNibbles: ByteString, child: Either[MptHash, MptValue]) extends MptNode {
    override def toString: String = {
      s"""MptExtension{
         |key nibbles: $keyNibbles
         |key nibbles length: ${keyNibbles.length}
         |key: ${Hex.toHexString(keyNibbles.toArray[Byte])}
         |childHash: ${child.fold(a => s"Hash(${Hex.toHexString(a.hash.toArray[Byte])})", b => s"Value(${Hex.toHexString(b.value.toArray[Byte])})")}
         |}
       """.stripMargin
    }
  }

  case class MptLeaf(keyNibbles: ByteString, value: ByteString) extends MptNode {

    def getAccount: Account = rlpDecode[Account](value.toArray[Byte])

    override def toString: String = {
      s"""MptLeaf{
         |key nibbles: $keyNibbles
         |key nibbles length: ${keyNibbles.length}
         |key: ${Hex.toHexString(keyNibbles.toArray[Byte])}
         |value: ${Hex.toHexString(value.toArray[Byte])}
         |}
       """.stripMargin
    }
  }

  case class MptHash(hash: ByteString)

  case class MptValue(value: ByteString)

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
        RLPList(loggerAddress, logTopics, data)
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
        RLPList(postTransactionStateHash, cumulativeGasUsed,
          logsBloomFilter, logs)
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
        RLPList(receiptsForBlocks.map(r => r:RLPList): _*)
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
