package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Account, Address, Receipt, TxLogEntry}
import io.iohk.ethereum.mpt.HexPrefix.{decode => hpDecode, encode => hpEncode}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable, MessageSerializableImplicit}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}
import org.spongycastle.util.encoders.Hex

import scala.language.implicitConversions


object PV63 {

  object GetNodeData {

    val code: Int = Versions.SubProtocolOffset + 0x0d

    implicit class GetNodeDataEnc(m: GetNodeData) extends MessageSerializableImplicit[GetNodeData](m) with RLPSerializable {
      override def code: Int = GetNodeData.code

      override def toRLPEncodable: RLPEncodeable = toRlpList(msg.mptElementsHashes)
    }

    implicit class GetNodeDataDec(val bytes: Array[Byte]) extends AnyVal {
      def toGetNodeData: GetNodeData = rawDecode(bytes) match {
        case rlpList: RLPList => GetNodeData(fromRlpList[ByteString](rlpList))
        case _ => throw new RuntimeException("Cannot decode GetNodeData")
      }
    }
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
    import UInt256RLPImplicits._

    implicit class AccountEnc(val account: Account) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import account._
        RLPList(nonce.toRLPEncodable, balance.toRLPEncodable, storageRoot, codeHash)
      }
    }

    implicit class AccountDec(val bytes: Array[Byte]) extends AnyVal {
      def toAccount: Account = rawDecode(bytes) match {
        case RLPList(nonce, balance, storageRoot, codeHash) =>
          Account(nonce.toUInt256, balance.toUInt256, storageRoot, codeHash)
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

    implicit class MptNodeEnc(obj: MptNode) extends RLPSerializable {

      def toRLPEncodable: RLPEncodeable = obj match {
        case n: MptLeaf =>
          import n._
          RLPList(RLPValue(hpEncode(keyNibbles.toArray[Byte], isLeaf = true)), value)
        case n: MptExtension =>
          import n._
          RLPList(RLPValue(hpEncode(keyNibbles.toArray[Byte], isLeaf = false)),
            child.fold(hash => hash.hash, node => node.toRLPEncodable)
          )
        case n: MptBranch =>
          import n._
          RLPList(children.map { e =>
            e.fold(mptHash => toEncodeable(mptHash.hash), node => node.toRLPEncodable)
          } :+ (value: RLPEncodeable): _*)
      }
    }

    implicit class MptNodeDec(val bytes: Array[Byte]) extends AnyVal {
      def toMptNode: MptNode = MptNodeRLPEncodableDec(rawDecode(bytes)).toMptNode
    }

    implicit class MptNodeRLPEncodableDec(val rlp: RLPEncodeable) extends AnyVal {
      def toMptNode: MptNode = rlp match {
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
            Right(list.toMptNode)

          case list: RLPList if list.items.length == BranchNodeChildLength + 1 && encodedLength <= MaxNodeValueSize =>
            Right(list.toMptNode)

          case _ => throw new RuntimeException("unexpected value in node")
        }
      }
    }
  }

  sealed trait MptNode {
    lazy val hash: ByteString = ByteString(kec256(this.toBytes: Array[Byte]))
  }

  object NodeData {

    val code: Int = Versions.SubProtocolOffset + 0x0e

    implicit class NodeDataEnc(m: NodeData) extends MessageSerializableImplicit[NodeData](m) with RLPSerializable {

      import io.iohk.ethereum.network.p2p.messages.PV63.MptNode._

      override def code: Int = NodeData.code
      override def toRLPEncodable: RLPEncodeable = msg.values

      @throws[RLPException]
      def getMptNode(index: Int): MptNode = msg.values(index).toArray[Byte].toMptNode
    }

    implicit class NodeDataDec(val bytes: Array[Byte]) extends AnyVal {
      def toNodeData: NodeData = rawDecode(bytes) match {
        case rlpList: RLPList => NodeData(rlpList.items.map { e => e: ByteString })
        case _ => throw new RuntimeException("Cannot decode NodeData")
      }
    }
  }

  case class NodeData(values: Seq[ByteString]) extends Message {

    override def code: Int = NodeData.code

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

    def getAccount: Account = value.toArray[Byte].toAccount

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
    val code: Int = Versions.SubProtocolOffset + 0x0f

    implicit class GetReceiptsEnc(m: GetReceipts) extends MessageSerializableImplicit[GetReceipts](m) with RLPSerializable {
      override def code: Int = GetReceipts.code

      override def toRLPEncodable: RLPEncodeable = msg.blockHashes: RLPList
    }

    implicit class GetReceiptsDec(val bytes: Array[Byte]) extends AnyVal {
      def toGetReceipts: GetReceipts = rawDecode(bytes) match {
        case rlpList: RLPList => GetReceipts(fromRlpList[ByteString](rlpList))
        case _ => throw new RuntimeException("Cannot decode GetReceipts")
      }
    }
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

    implicit class TxLogEntryEnc(logEntry: TxLogEntry) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import logEntry._
        RLPList(loggerAddress.bytes, logTopics, data)
      }
    }

    implicit class TxLogEntryDec(rlp: RLPEncodeable) {
      def toTxLogEntry: TxLogEntry = rlp match {
        case RLPList(loggerAddress, logTopics: RLPList, data) =>
          TxLogEntry(Address(loggerAddress: ByteString), fromRlpList[ByteString](logTopics), data)

        case _ => throw new RuntimeException("Cannot decode TransactionLog")
      }
    }
  }

  object ReceiptImplicits {
    import TxLogEntryImplicits._

    implicit class ReceiptEnc(msg: Receipt) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        RLPList(postTransactionStateHash, cumulativeGasUsed, logsBloomFilter, RLPList(logs.map(_.toRLPEncodable): _*))
      }
    }

    implicit class ReceiptSeqEnc(receipts: Seq[Receipt]) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = RLPList(receipts.map(_.toRLPEncodable): _*)
    }

    implicit class ReceiptDec(val bytes: Array[Byte]) extends AnyVal {
      def toReceipt: Receipt = ReceiptRLPEncodableDec(rawDecode(bytes)).toReceipt

      def toReceipts: Seq[Receipt] = rawDecode(bytes) match {
        case RLPList(items@_*) => items.map(_.toReceipt)
        case _ => throw new RuntimeException("Cannot decode Receipts")
      }
    }

    implicit class ReceiptRLPEncodableDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {
      def toReceipt: Receipt = rlpEncodeable match {
        case RLPList(postTransactionStateHash, cumulativeGasUsed, logsBloomFilter, logs: RLPList) =>
          Receipt(postTransactionStateHash, cumulativeGasUsed, logsBloomFilter, logs.items.map(_.toTxLogEntry))
        case _ => throw new RuntimeException("Cannot decode Receipt")
      }
    }
  }

  object Receipts {

    val code: Int = Versions.SubProtocolOffset + 0x10

    implicit class ReceiptsEnc(m: Receipts) extends MessageSerializableImplicit[Receipts](m) with RLPSerializable {
      import ReceiptImplicits._

      override def code: Int = Receipts.code

      override def toRLPEncodable: RLPEncodeable = RLPList(
        msg.receiptsForBlocks.map( (rs: Seq[Receipt]) =>
          RLPList(rs.map((r: Receipt) => r.toRLPEncodable): _*)
        ): _*
      )
    }

    implicit class ReceiptsDec(val bytes: Array[Byte]) extends AnyVal {
      import ReceiptImplicits._

      def toReceipts: Receipts = rawDecode(bytes) match {
        case rlpList: RLPList => Receipts(rlpList.items.collect { case r: RLPList => r.items.map(_.toReceipt) })
        case _ => throw new RuntimeException("Cannot decode Receipts")
      }
    }
  }

  case class Receipts(receiptsForBlocks: Seq[Seq[Receipt]]) extends Message {
    override def code: Int = Receipts.code
  }
}
