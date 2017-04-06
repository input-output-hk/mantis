package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Account, Address, Receipt, TxLogEntry}
import io.iohk.ethereum.mpt.HexPrefix.{decode => hpDecode, encode => hpEncode}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode, _}
import org.spongycastle.util.encoders.Hex


object PV63 {

  object GetNodeData {
    implicit val rlpEncDec = new RLPEncoder[GetNodeData] with RLPDecoder[GetNodeData] {
      override def encode(obj: GetNodeData): RLPEncodeable = {
        import obj._
        toRlpList(mptElementsHashes)
      }

      override def decode(rlp: RLPEncodeable): GetNodeData = rlp match {
        case rlpList: RLPList => GetNodeData(fromRlpList[ByteString](rlpList))
        case _ => throw new RuntimeException("Cannot decode GetNodeData")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x0d
  }

  case class GetNodeData(mptElementsHashes: Seq[ByteString]) extends Message {
    override def code: Int = GetNodeData.code

    override def toString: String = {
      s"""GetNodeData{
         |hashes: ${mptElementsHashes.map(e => Hex.toHexString(e.toArray[Byte]))}
         |}
       """.stripMargin
    }
  }

  object AccountImplicits {
    implicit val rlpEncDec = new RLPEncoder[Account] with RLPDecoder[Account] {
      override def encode(obj: Account): RLPEncodeable = {
        import obj._
        RLPList(nonce, balance, storageRoot, codeHash)
      }

      override def decode(rlp: RLPEncodeable): Account = rlp match {
        case RLPList(nonce, balance, storageRoot, codeHash) =>
          Account(nonce, balance, storageRoot, codeHash)
        case _ => throw new RuntimeException("Cannot decode Account")
      }
    }
  }

  object MptNode {
    val BranchNodeChildLength = 16
    val BranchNodeIndexOfValue = 16
    val ExtensionNodeLength = 2
    val LeafNodeLength = 2
    val MaxNodeValueSize = 31
    val HashLength = 32

    implicit val rlpEncDec = new RLPEncoder[MptNode] with RLPDecoder[MptNode] {
      override def encode(obj: MptNode): RLPEncodeable = {
        obj match {
          case n: MptLeaf =>
            import n._
            RLPList(RLPValue(hpEncode(keyNibbles.toArray[Byte], isLeaf = true)), value)
          case n: MptExtension =>
            import n._
            RLPList(RLPValue(hpEncode(keyNibbles.toArray[Byte], isLeaf = false)),
              child.fold(hash => hash.hash, node => encode(node))
            )
          case n: MptBranch =>
            import n._
            RLPList(children.map { e =>
              e.fold(mptHash => toEncodeable(mptHash.hash), node => encode(node))
            } :+ (value: RLPEncodeable): _*)
        }
      }

      override def decode(rlp: RLPEncodeable): MptNode = rlp match {
        case rlpList: RLPList if rlpList.items.length == BranchNodeChildLength + 1 =>
          MptBranch(rlpList.items.take(BranchNodeChildLength).map(decodeChild), byteStringEncDec.decode(rlpList.items(BranchNodeIndexOfValue)))
        case RLPList(hpEncoded, value) =>
          hpDecode(hpEncoded: Array[Byte]) match {
            case (decoded, true) =>
              MptLeaf(ByteString(decoded), value)
            case (decoded, false) =>
              MptExtension(ByteString(decoded), decodeChild(value))
          }
        case _ =>
          throw new RuntimeException("Cannot decode NodeData")
      }

      private def decodeChild(rlp: RLPEncodeable): Either[MptHash, MptNode] = {
        val encodedLength = rlpEncode(rlp).length

        rlp match {
          case bytes: RLPValue if bytes.bytes.length == HashLength || bytes.bytes.length == 0 =>
            Left(MptHash(bytes))

          case list: RLPList if (list.items.length == ExtensionNodeLength || list.items.length == LeafNodeLength) && encodedLength <= MaxNodeValueSize =>
            Right(decode(list))

          case list: RLPList if list.items.length == BranchNodeChildLength + 1 && encodedLength <= MaxNodeValueSize =>
            Right(decode(list))

          case _ => throw new RuntimeException("unexpected value in node")
        }
      }
    }
  }

  sealed trait MptNode {
    lazy val hash: ByteString = ByteString(kec256(rlpEncode(this)))
  }

  object NodeData {
    implicit val rlpEncDec = new RLPEncoder[NodeData] with RLPDecoder[NodeData] {
      override def encode(obj: NodeData): RLPEncodeable = {
        import obj._
        values
      }

      override def decode(rlp: RLPEncodeable): NodeData = rlp match {
        case rlpList: RLPList =>
          NodeData(rlpList.items.map { e => e: ByteString })
        case _ => throw new RuntimeException("Cannot decode NodeData")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x0e
  }

  case class NodeData(values: Seq[ByteString]) extends Message {
    override def code: Int = NodeData.code

    @throws[RLPException]
    def getMptNode(index: Int): MptNode = rlpDecode[MptNode](values(index).toArray[Byte])

    override def toString: String = {
      s"""NodeData{
         |values: ${values.map(b => Hex.toHexString(b.toArray[Byte]))}
         |}
       """.stripMargin
    }
  }

  case class MptBranch(children: Seq[Either[MptHash, MptNode]], value: ByteString) extends MptNode {
    require(children.length == 16, "MptBranch childHashes length have to be 16")

    override def toString: String = {
      val childrenString = children.map { e =>
        e.fold(
          { hash => s"Hash(${Hex.toHexString(hash.hash.toArray[Byte])})" },
          { node => s"Value(${node.toString})" })
      }.mkString("(", ",\n", ")")

      s"""MptBranch{
         |children: $childrenString
         |value: ${Hex.toHexString(value.toArray[Byte])}
         |}
       """.stripMargin
    }
  }

  case class MptExtension(keyNibbles: ByteString, child: Either[MptHash, MptNode]) extends MptNode {
    override def toString: String = {
      s"""MptExtension{
         |key nibbles: $keyNibbles
         |key nibbles length: ${keyNibbles.length}
         |key: ${Hex.toHexString(keyNibbles.toArray[Byte])}
         |childHash: s"Hash(${child.fold({ hash => Hex.toHexString(hash.hash.toArray[Byte]) }, { node => node.toString })})"
         |}
       """.stripMargin
    }
  }

  case class MptLeaf(keyNibbles: ByteString, value: ByteString) extends MptNode {

    import AccountImplicits._

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

  object GetReceipts {
    implicit val rlpEncDec = new RLPEncoder[GetReceipts] with RLPDecoder[GetReceipts] {
      override def encode(obj: GetReceipts): RLPEncodeable = {
        import obj._
        blockHashes: RLPList
      }

      override def decode(rlp: RLPEncodeable): GetReceipts = rlp match {
        case rlpList: RLPList => GetReceipts(fromRlpList[ByteString](rlpList))
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

  object TxLogEntryImplicits {
    implicit val rlpEncDec = new RLPEncoder[TxLogEntry] with RLPDecoder[TxLogEntry] {
      override def encode(obj: TxLogEntry): RLPEncodeable = {
        import obj._
        RLPList(loggerAddress.bytes, logTopics, data)
      }

      override def decode(rlp: RLPEncodeable): TxLogEntry = rlp match {
        case RLPList(loggerAddress, logTopics: RLPList, data) =>
          TxLogEntry(Address(loggerAddress: ByteString), fromRlpList[ByteString](logTopics), data)

        case _ => throw new RuntimeException("Cannot decode TransactionLog")
      }
    }
  }

  object ReceiptImplicits {
    implicit val receiptRlpEncDec = new RLPEncoder[Receipt] with RLPDecoder[Receipt] {
      override def encode(obj: Receipt): RLPEncodeable = {
        import obj._
        RLPList(postTransactionStateHash, cumulativeGasUsed, logsBloomFilter, toRlpList[TxLogEntry](logs)(TxLogEntryImplicits.rlpEncDec))
      }

      override def decode(rlp: RLPEncodeable): Receipt = rlp match {
        case RLPList(postTransactionStateHash, cumulativeGasUsed, logsBloomFilter, logs: RLPList) =>
          Receipt(postTransactionStateHash, cumulativeGasUsed, logsBloomFilter, fromRlpList[TxLogEntry](logs)(TxLogEntryImplicits.rlpEncDec))
        case _ => throw new RuntimeException("Cannot decode Receipt")
      }
    }
  }

  object Receipts {
    import ReceiptImplicits.receiptRlpEncDec

    implicit val rlpEncDec = new RLPEncoder[Receipts] with RLPDecoder[Receipts] {
      override def encode(obj: Receipts): RLPEncodeable = {
        import obj._
        RLPList(receiptsForBlocks.map(toRlpList[Receipt](_)(receiptRlpEncDec)): _*)
      }

      override def decode(rlp: RLPEncodeable): Receipts = rlp match {
        case rlpList: RLPList => Receipts(rlpList.items.collect { case r: RLPList => fromRlpList[Receipt](r)(receiptRlpEncDec) })
        case _ => throw new RuntimeException("Cannot decode Receipts")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x10
  }

  case class Receipts(receiptsForBlocks: Seq[Seq[Receipt]]) extends Message {
    override def code: Int = Receipts.code
  }
}
