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
        msg.code match {
          case `code63` =>
            RLPList(protocolVersion, networkId, chainWeight.totalDifficulty, bestHash, genesisHash)

          case `code64` =>
            RLPList(
              protocolVersion,
              networkId,
              chainWeight.totalDifficulty,
              chainWeight.lastCheckpointNumber,
              bestHash,
              genesisHash
            )
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
          Status(
            code63,
            protocolVersion,
            networkId,
            ChainWeight.totalDifficultyOnly(totalDifficulty),
            bestHash,
            genesisHash
          )

        case (
              `code64`,
              RLPList(
                protocolVersion,
                networkId,
                totalDifficulty,
                lastCheckpointNumber,
                bestHash,
                genesisHash
              )
            ) =>
          Status(
            code64,
            protocolVersion,
            networkId,
            ChainWeight(lastCheckpointNumber, totalDifficulty),
            bestHash,
            genesisHash
          )

        case _ => throw new RuntimeException("Cannot decode Status")
      }
    }

    /**
      * Constructs the message with it specifying the code. The code should be regarded as undefined at this stage.
      * It should be later made concrete with `as63` or `as64` methods.
      *
      * FIXME this approach was taken to minimise the required refactoring and should be reconsidered in ETCM-280
      */
    def apply(
        protocolVersion: Int,
        networkId: Int,
        chainWeight: ChainWeight,
        bestHash: ByteString,
        genesisHash: ByteString
    ): Status =
      Status(Status.code63, protocolVersion, networkId, chainWeight, bestHash, genesisHash)

  }

  case class Status(
      code: Int,
      protocolVersion: Int,
      networkId: Int,
      chainWeight: ChainWeight,
      bestHash: ByteString,
      genesisHash: ByteString
  ) extends Message {
    require(code == Status.code63 || code == Status.code64, s"Invalid code for Status: $code")

    override def toString: String = {
      s"""Status {
         |code: $code
         |protocolVersion: $protocolVersion
         |networkId: $networkId
         |chainWeight: $chainWeight
         |bestHash: ${Hex.toHexString(bestHash.toArray[Byte])}
         |genesisHash: ${Hex.toHexString(genesisHash.toArray[Byte])}
         |}""".stripMargin
    }

    def as63: Status =
      copy(code = Status.code63)

    def as64: Status =
      copy(code = Status.code64)
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
        msg.code match {
          case `code63` =>
            RLPList(
              RLPList(
                block.header.toRLPEncodable,
                RLPList(block.body.transactionList.map(_.toRLPEncodable): _*),
                RLPList(block.body.uncleNodesList.map(_.toRLPEncodable): _*)
              ),
              chainWeight.totalDifficulty
            )

          case `code64` =>
            RLPList(
              RLPList(
                block.header.toRLPEncodable,
                RLPList(block.body.transactionList.map(_.toRLPEncodable): _*),
                RLPList(block.body.uncleNodesList.map(_.toRLPEncodable): _*)
              ),
              chainWeight.totalDifficulty,
              chainWeight.lastCheckpointNumber
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
          NewBlock(
            code63,
            Block(
              blockHeader.toBlockHeader,
              BlockBody(transactionList.items.map(_.toSignedTransaction), uncleNodesList.items.map(_.toBlockHeader))
            ),
            ChainWeight.totalDifficultyOnly(totalDifficulty)
          )

        case (
              `code64`,
              RLPList(
                RLPList(blockHeader, (transactionList: RLPList), (uncleNodesList: RLPList)),
                totalDifficulty,
                lastCheckpointNumber
              )
            ) =>
          NewBlock(
            code64,
            Block(
              blockHeader.toBlockHeader,
              BlockBody(transactionList.items.map(_.toSignedTransaction), uncleNodesList.items.map(_.toBlockHeader))
            ),
            ChainWeight(lastCheckpointNumber, totalDifficulty)
          )
        case _ => throw new RuntimeException("Cannot decode NewBlock")
      }
    }

    /**
      * Constructs the message with it specifying the code. The code should be regarded as undefined at this stage.
      * It should be later made concrete with `as63` or `as64` methods.
      *
      * FIXME this approach was taken to minimise the required refactoring and should be reconsidered in ETCM-280
      */
    def apply(block: Block, chainWeight: ChainWeight): NewBlock =
      NewBlock(NewBlock.code63, block, chainWeight)

  }

  case class NewBlock(code: Int, block: Block, chainWeight: ChainWeight) extends Message {
    require(code == NewBlock.code63 || code == NewBlock.code64, s"Invalid code for NewBlock: $code")

    override def toString: String = {
      s"""NewBlock {
         |code: $code
         |block: $block
         |chainWeight: $chainWeight
         |}""".stripMargin
    }

    def as63: NewBlock =
      copy(code = NewBlock.code63)

    def as64: NewBlock =
      copy(code = NewBlock.code64)
  }
}
