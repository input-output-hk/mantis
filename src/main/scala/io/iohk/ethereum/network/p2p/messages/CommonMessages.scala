package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.network.p2p.{Message, MessageSerializableImplicit}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.utils.Config
import org.bouncycastle.util.encoders.Hex

object CommonMessages {
  object Status {
    val code63: Int = Versions.SubProtocolOffset + 0x00
    val code64: Int = Versions.SubProtocolOffset + 0x11

    implicit class StatusEnc(val underlyingMsg: Status)
        extends MessageSerializableImplicit[Status](underlyingMsg)
        with RLPSerializable {
      override def code: Int = underlyingMsg.code

      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        msg match {
          case _: Status63 =>
            RLPList(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash)

          case _: Status64 =>
            RLPList(protocolVersion, networkId, totalDifficulty, latestCheckpointNumber, bestHash, genesisHash)
        }
      }
    }

    implicit class StatusDec(val bytes: Array[Byte]) extends AnyVal {
      def toStatus(code: Int): Status = (code, rawDecode(bytes)) match {
        case (
              `code63`,
              RLPList(
                protocolVersion,
                networkId,
                totalDifficulty,
                bestHash,
                genesisHash
              )
            ) =>
          Status63(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash)

        case (
              `code64`,
              RLPList(
                protocolVersion,
                networkId,
                totalDifficulty,
                latestCheckpointNumber,
                bestHash,
                genesisHash
              )
            ) =>
          Status64(protocolVersion, networkId, totalDifficulty, latestCheckpointNumber, bestHash, genesisHash)

        case _ => throw new RuntimeException("Cannot decode Status")
      }
    }

    def apply(
        protocolVersion: Int,
        networkId: Int,
        totalDifficulty: BigInt,
        bestHash: ByteString,
        genesisHash: ByteString,
        latestCheckpointNumber: Option[BigInt] = None
    ): Status = latestCheckpointNumber match {
      case Some(num) =>
        Status64(protocolVersion, networkId, totalDifficulty, num, bestHash, genesisHash)

      case None =>
        Status63(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash)
    }
  }

  sealed trait Status extends Message {
    def protocolVersion: Int
    def networkId: Int
    def totalDifficulty: BigInt
    def latestCheckpointNumber: BigInt
    def bestHash: ByteString
    def genesisHash: ByteString

    // Test API
    def as63: Status63 =
      Status63(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash)

    def as64: Status64 =
      Status64(protocolVersion, networkId, totalDifficulty, latestCheckpointNumber, bestHash, genesisHash)
  }

  case class Status63(
      protocolVersion: Int,
      networkId: Int,
      totalDifficulty: BigInt,
      bestHash: ByteString,
      genesisHash: ByteString
  ) extends Status {
    override val code: Int = Status.code63

    override def toString: String = {
      s"""Status63 {
         |protocolVersion: $protocolVersion
         |networkId: $networkId
         |totalDifficulty: $totalDifficulty
         |bestHash: ${Hex.toHexString(bestHash.toArray[Byte])}
         |genesisHash: ${Hex.toHexString(genesisHash.toArray[Byte])}
         |}""".stripMargin
    }

    override val latestCheckpointNumber: BigInt = 0
  }

  case class Status64(
      protocolVersion: Int,
      networkId: Int,
      totalDifficulty: BigInt,
      latestCheckpointNumber: BigInt,
      bestHash: ByteString,
      genesisHash: ByteString
  ) extends Status {
    override val code: Int = Status.code64

    override def toString: String = {
      s"""Status64 {
         |protocolVersion: $protocolVersion
         |networkId: $networkId
         |totalDifficulty: $totalDifficulty
         |bestHash: ${Hex.toHexString(bestHash.toArray[Byte])}
         |genesisHash: ${Hex.toHexString(genesisHash.toArray[Byte])}
         |latestCheckpointNumber: $latestCheckpointNumber
         |}""".stripMargin
    }
  }

  object SignedTransactions {

    lazy val chainId: Byte = Config.blockchains.blockchainConfig.chainId

    val code: Int = Versions.SubProtocolOffset + 0x02

    implicit class SignedTransactionEnc(val signedTx: SignedTransaction) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        val receivingAddressBytes = signedTx.tx.receivingAddress
          .map(_.toArray)
          .getOrElse(Array.emptyByteArray)
        RLPList(
          signedTx.tx.nonce,
          signedTx.tx.gasPrice,
          signedTx.tx.gasLimit,
          receivingAddressBytes,
          signedTx.tx.value,
          signedTx.tx.payload,
          signedTx.signature.v,
          signedTx.signature.r,
          signedTx.signature.s
        )
      }
    }

    implicit class SignedTransactionsEnc(val underlyingMsg: SignedTransactions)
        extends MessageSerializableImplicit[SignedTransactions](underlyingMsg)
        with RLPSerializable {

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
        case RLPList(
              nonce,
              gasPrice,
              gasLimit,
              (receivingAddress: RLPValue),
              value,
              payload,
              pointSign,
              signatureRandom,
              signature
            ) =>
          val receivingAddressOpt = if (receivingAddress.bytes.isEmpty) None else Some(Address(receivingAddress.bytes))
          SignedTransaction(
            Transaction(nonce, gasPrice, gasLimit, receivingAddressOpt, value, payload),
            (pointSign: Int).toByte,
            signatureRandom,
            signature,
            chainId
          )
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

    val code63: Int = Versions.SubProtocolOffset + 0x07
    val code64: Int = Versions.SubProtocolOffset + 0x12

    implicit class NewBlockEnc(val underlyingMsg: NewBlock)
        extends MessageSerializableImplicit[NewBlock](underlyingMsg)
        with RLPSerializable {
      import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

      override def code: Int = msg.code

      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        msg match {
          case _: NewBlock63 =>
            RLPList(
              RLPList(
                block.header.toRLPEncodable,
                RLPList(block.body.transactionList.map(_.toRLPEncodable): _*),
                RLPList(block.body.uncleNodesList.map(_.toRLPEncodable): _*)
              ),
              totalDifficulty
            )

          case _: NewBlock64 =>
            RLPList(
              RLPList(
                block.header.toRLPEncodable,
                RLPList(block.body.transactionList.map(_.toRLPEncodable): _*),
                RLPList(block.body.uncleNodesList.map(_.toRLPEncodable): _*)
              ),
              totalDifficulty,
              latestCheckpointNumber
            )
        }

      }
    }

    implicit class NewBlockDec(val bytes: Array[Byte]) extends AnyVal {
      import SignedTransactions._

      def toNewBlock(code: Int): NewBlock = (code, rawDecode(bytes)) match {
        case (
              `code63`,
              RLPList(RLPList(blockHeader, (transactionList: RLPList), (uncleNodesList: RLPList)), totalDifficulty)
            ) =>
          NewBlock63(
            Block(
              blockHeader.toBlockHeader,
              BlockBody(transactionList.items.map(_.toSignedTransaction), uncleNodesList.items.map(_.toBlockHeader))
            ),
            totalDifficulty
          )

        case (
              `code64`,
              RLPList(
                RLPList(blockHeader, (transactionList: RLPList), (uncleNodesList: RLPList)),
                totalDifficulty,
                latestCheckpointNumber
              )
            ) =>
          NewBlock64(
            Block(
              blockHeader.toBlockHeader,
              BlockBody(transactionList.items.map(_.toSignedTransaction), uncleNodesList.items.map(_.toBlockHeader))
            ),
            totalDifficulty,
            latestCheckpointNumber
          )
        case _ => throw new RuntimeException("Cannot decode NewBlock")
      }
    }

    def apply(block: Block, totalDifficulty: BigInt, latestCheckpointNumber: Option[BigInt] = None): NewBlock =
      latestCheckpointNumber match {
        case Some(num) => NewBlock64(block, totalDifficulty, num)
        case None => NewBlock63(block, totalDifficulty)
      }

    def unapply(nb: NewBlock): Option[(Block, BigInt, BigInt)] =
      Some((nb.block, nb.totalDifficulty, nb.latestCheckpointNumber))

  }

  sealed trait NewBlock extends Message {
    def block: Block
    def totalDifficulty: BigInt
    def latestCheckpointNumber: BigInt

    // Test API
    def as63: NewBlock63 =
      NewBlock63(block, totalDifficulty)

    def as64: NewBlock64 =
      NewBlock64(block, totalDifficulty, latestCheckpointNumber)
  }

  case class NewBlock63(block: Block, totalDifficulty: BigInt) extends NewBlock {
    override val code: Int = NewBlock.code63

    override def toString: String = {
      s"""NewBlock63 {
         |block: $block
         |totalDifficulty: $totalDifficulty
         |}""".stripMargin
    }

    override val latestCheckpointNumber: BigInt = 0
  }

  case class NewBlock64(block: Block, totalDifficulty: BigInt, latestCheckpointNumber: BigInt) extends NewBlock {
    override val code: Int = NewBlock.code64

    override def toString: String = {
      s"""NewBlock64 {
         |block: $block
         |totalDifficulty: $totalDifficulty
         |latestCheckpointNumber: $latestCheckpointNumber
         |}""".stripMargin
    }
  }
}
