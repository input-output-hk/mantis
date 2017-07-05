package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.{Message, MessageSerializableImplicit}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import org.spongycastle.util.encoders.Hex


object CommonMessages {
  object Status {
    val code: Int = Versions.SubProtocolOffset + 0x00

    implicit class StatusEnc(val underlyingMsg: Status) extends MessageSerializableImplicit[Status](underlyingMsg) with RLPSerializable {
      override def code: Int = Status.code

      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        RLPList(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash)
      }
    }

    implicit class StatusDec(val bytes: Array[Byte]) extends AnyVal {
      def toStatus: Status = rawDecode(bytes) match {
        case RLPList(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash) =>
          Status(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash)
        case _ => throw new RuntimeException("Cannot decode Status")
      }
    }
  }

  case class Status(protocolVersion: Int, networkId: Int, totalDifficulty: BigInt, bestHash: ByteString, genesisHash: ByteString) extends Message {
    override def code: Int = Status.code

    override def toString: String = {
      s"""Status {
         |protocolVersion: $protocolVersion
         |networkId: $networkId
         |totalDifficulty: $totalDifficulty
         |bestHash: ${Hex.toHexString(bestHash.toArray[Byte])}
         |genesisHash: ${Hex.toHexString(genesisHash.toArray[Byte])}
         |}""".stripMargin
    }
  }

  object SignedTransactions {

    val chainId: Byte = BlockchainConfig(Config.config).chainId

    val code: Int = Versions.SubProtocolOffset + 0x02

    implicit class SignedTransactionEnc(val signedTx: SignedTransaction) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import signedTx._
        import signedTx.tx._
        RLPList(nonce, gasPrice, gasLimit, receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray): Array[Byte], value,
          payload, signature.v, signature.r, signature.s)
      }
    }

    implicit class SignedTransactionsEnc(val underlyingMsg: SignedTransactions)
      extends MessageSerializableImplicit[SignedTransactions](underlyingMsg) with RLPSerializable {

      override def code: Int = SignedTransactions.code
      override def toRLPEncodable: RLPEncodeable = RLPList(msg.txs.map(_.toRLPEncodable): _*)
    }

    implicit class SignedTransactionsDec(val bytes: Array[Byte]) extends AnyVal {
      def toSignedTransactions: SignedTransactions = rawDecode(bytes) match {
        case rlpList: RLPList => SignedTransactions(rlpList.items.map(_.toSignedTransaction))
        case _ => throw new RuntimeException("Cannot decode SignedTransactions")
      }
    }

    implicit class SignedTransactionRlpEncodableDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {
      def toSignedTransaction: SignedTransaction = rlpEncodeable match {
        case RLPList(nonce, gasPrice, gasLimit, (receivingAddress: RLPValue), value,
        payload, pointSign, signatureRandom, signature) =>
          val receivingAddressOpt = if(receivingAddress.bytes.isEmpty) None else Some(Address(receivingAddress.bytes))
          SignedTransaction(
            Transaction(nonce, gasPrice, gasLimit, receivingAddressOpt, value, payload),
            (pointSign: Int).toByte,
            signatureRandom,
            signature,
            chainId
          ).getOrElse(throw new Exception("Tx with invalid signature"))
      }
    }

    implicit class SignedTransactionDec(val bytes: Array[Byte]) extends AnyVal {
      def toSignedTransaction: SignedTransaction = rawDecode(bytes).toSignedTransaction
    }
  }

  case class SignedTransactions(txs: Seq[SignedTransaction]) extends Message {
    override def code: Int = SignedTransactions.code
  }

  object NewBlock {

    val code: Int = Versions.SubProtocolOffset + 0x07

    implicit class NewBlockEnc(val underlyingMsg: NewBlock) extends MessageSerializableImplicit[NewBlock](underlyingMsg) with RLPSerializable {
      import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._
      import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._

      override def code: Int = NewBlock.code

      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        RLPList(
          RLPList(
            block.header.toRLPEncodable,
            RLPList(block.body.transactionList.map(_.toRLPEncodable): _*),
            RLPList(block.body.uncleNodesList.map(_.toRLPEncodable): _*)
          ),
          totalDifficulty
        )
      }
    }

    implicit class NewBlockDec(val bytes: Array[Byte]) extends AnyVal {
      import io.iohk.ethereum.network.p2p.messages.PV62._
      import BlockHeaderImplicits._
      import SignedTransactions._

      def toNewBlock: NewBlock = rawDecode(bytes) match {
        case RLPList(RLPList(blockHeader, (transactionList: RLPList), (uncleNodesList: RLPList)), totalDifficulty) =>
          NewBlock(
            Block(
              blockHeader.toBlockHeader,
              BlockBody(
                transactionList.items.map(_.toSignedTransaction),
                uncleNodesList.items.map(_.toBlockHeader))
            ),
            totalDifficulty
          )
        case _ => throw new RuntimeException("Cannot decode NewBlock")
      }
    }
  }

  case class NewBlock(block: Block, totalDifficulty: BigInt) extends Message {
    override def code: Int = NewBlock.code

    override def toString: String = {
      s"""NewBlock {
         |block: $block
         |totalDifficulty: $totalDifficulty
         |}""".stripMargin
    }
  }
}
