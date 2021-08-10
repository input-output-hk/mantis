package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex

import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.MessageSerializableImplicit
import io.iohk.ethereum.rlp.RLPCodec.Ops
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.utils.ByteStringUtils.ByteStringOps

object BaseETH6XMessages {
  object Status {
    implicit class StatusEnc(val underlyingMsg: Status)
        extends MessageSerializableImplicit[Status](underlyingMsg)
        with RLPSerializable {
      override def code: Int = Codes.StatusCode

      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        RLPList(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash)
      }
    }

    implicit class StatusDec(val bytes: Array[Byte]) extends AnyVal {
      def toStatus: Status = rawDecode(bytes) match {
        case RLPList(
              protocolVersion,
              networkId,
              totalDifficulty,
              bestHash,
              genesisHash
            ) =>
          Status(
            protocolVersion,
            networkId,
            totalDifficulty,
            bestHash,
            genesisHash
          )

        case _ => throw new RuntimeException("Cannot decode Status")
      }
    }

  }

  implicit val addressCodec: RLPCodec[Address] =
    implicitly[RLPCodec[Array[Byte]]].xmap(Address(_), _.toArray)

  implicit val accessListItemCodec: RLPCodec[AccessListItem] =
    RLPCodec.instance[AccessListItem](
      { case AccessListItem(address, storageKeys) =>
        RLPList(address, toRlpList(storageKeys))
      },
      {
        case r: RLPList if r.items.isEmpty => AccessListItem(null, List.empty)

        case RLPList(rlpAddress, rlpStorageKeys: RLPList) =>
          val address = rlpAddress.decodeAs[Address]("address ")
          val storageKeys = fromRlpList[BigInt](rlpStorageKeys).toList
          AccessListItem(address, storageKeys)
      }
    )

  /** used by eth61, eth62, eth63
    */
  case class Status(
      protocolVersion: Int,
      networkId: Int,
      totalDifficulty: BigInt,
      bestHash: ByteString,
      genesisHash: ByteString
  ) extends Message {

    override def toString: String =
      s"Status { " +
        s"code: $code, " +
        s"protocolVersion: $protocolVersion, " +
        s"networkId: $networkId, " +
        s"totalDifficulty: $totalDifficulty, " +
        s"bestHash: ${Hex.toHexString(bestHash.toArray[Byte])}, " +
        s"genesisHash: ${Hex.toHexString(genesisHash.toArray[Byte])}," +
        s"}"

    override def toShortString: String = toString
    override def code: Int = Codes.StatusCode
  }

  object NewBlock {
    implicit class NewBlockEnc(val underlyingMsg: NewBlock)
        extends MessageSerializableImplicit[NewBlock](underlyingMsg)
        with RLPSerializable {
      import SignedTransactions._

      override def code: Int = Codes.NewBlockCode

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
      import SignedTransactions._

      def toNewBlock: NewBlock = rawDecode(bytes) match {
        case RLPList(RLPList(blockHeader, transactionList: RLPList, uncleNodesList: RLPList), totalDifficulty) =>
          NewBlock(
            Block(
              blockHeader.toBlockHeader,
              BlockBody(transactionList.items.map(_.toSignedTransaction), uncleNodesList.items.map(_.toBlockHeader))
            ),
            totalDifficulty
          )

        case _ => throw new RuntimeException("Cannot decode NewBlock")
      }
    }
  }

  /** used by eth61, eth62, eth63
    */
  case class NewBlock(block: Block, totalDifficulty: BigInt) extends Message {

    override def toString: String =
      s"NewBlock { " +
        s"code: $code, " +
        s"block: $block, " +
        s"totalDifficulty: $totalDifficulty" +
        s"}"

    override def toShortString: String =
      s"NewBlock { " +
        s"code: $code, " +
        s"block.header: ${block.header}, " +
        s"totalDifficulty: $totalDifficulty" +
        s"}"

    override def code: Int = Codes.NewBlockCode
  }

  object SignedTransactions {

    lazy val chainId: Byte = 1.toByte //Config.blockchains.blockchainConfig.chainId

    implicit class SignedTransactionEnc(val signedTx: SignedTransaction) extends RLPSerializable {

      override def toBytes: Array[Byte] =
        signedTx.tx match {
          // transaction with access list encoding is defined in eip2930
          case _: TransactionWithAccessList => Transaction.Type01 +: (super.toBytes: Array[Byte])
          case _: LegacyTransaction         => super.toBytes
        }

      override def toRLPEncodable: RLPEncodeable = {
        val receivingAddressBytes = signedTx.tx.receivingAddress
          .map(_.toArray)
          .getOrElse(Array.emptyByteArray)
        signedTx.tx match {
          case TransactionWithAccessList(nonce, gasPrice, gasLimit, _, value, payload, accessList) =>
            RLPList(
              chainId, // TODO improve how chainid is preserved in transactions
              nonce,
              gasPrice,
              gasLimit,
              receivingAddressBytes,
              value,
              payload,
              toRlpList(accessList),
              signedTx.signature.v,
              signedTx.signature.r,
              signedTx.signature.s
            )
          case LegacyTransaction(nonce, gasPrice, gasLimit, _, value, payload) =>
            RLPList(
              nonce,
              gasPrice,
              gasLimit,
              receivingAddressBytes,
              value,
              payload,
              signedTx.signature.v,
              signedTx.signature.r,
              signedTx.signature.s
            )
        }
      }
    }

    implicit class SignedTransactionsEnc(val underlyingMsg: SignedTransactions)
        extends MessageSerializableImplicit[SignedTransactions](underlyingMsg)
        with RLPSerializable {

      override def code: Int = Codes.SignedTransactionsCode
      override def toRLPEncodable: RLPEncodeable = RLPList(msg.txs.map(_.toRLPEncodable): _*)
    }

    implicit class SignedTransactionsDec(val bytes: Array[Byte]) extends AnyVal {
      def toSignedTransactions: SignedTransactions = rawDecode(bytes) match {
        case rlpList: RLPList => SignedTransactions(rlpList.items.map(_.toSignedTransaction))
        case _                => throw new RuntimeException("Cannot decode SignedTransactions")
      }
    }

    implicit class SignedTransactionRlpEncodableDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {

      def toSignedTransaction: SignedTransaction = rlpEncodeable match {
        case RLPList(
              _, // TODO improve how chainid is preserved in transactions
              nonce,
              gasPrice,
              gasLimit,
              (receivingAddress: RLPValue),
              value,
              payload,
              (accessList: RLPList),
              pointSign,
              signatureRandom,
              signature
            ) =>
          val receivingAddressOpt = if (receivingAddress.bytes.isEmpty) None else Some(Address(receivingAddress.bytes))
          SignedTransaction(
            TransactionWithAccessList(
              nonce,
              gasPrice,
              gasLimit,
              receivingAddressOpt,
              value,
              payload,
              fromRlpList[AccessListItem](accessList).toList
            ),
            (pointSign: Int).toByte,
            signatureRandom,
            signature
          )
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
            LegacyTransaction(nonce, gasPrice, gasLimit, receivingAddressOpt, value, payload),
            (pointSign: Int).toByte,
            signatureRandom,
            signature
          )
        case _ =>
          throw new RuntimeException("Cannot decode SignedTransaction")
      }
    }

    implicit class SignedTransactionDec(val bytes: Array[Byte]) extends AnyVal {
      def toSignedTransaction: SignedTransaction = {
        val first = bytes(0)
        (first match {
          case Transaction.Type01 => rawDecode(bytes.tail)
          // TODO enforce legacy boundaries
          case _ => rawDecode(bytes)
        }).toSignedTransaction
      }
    }
  }

  case class SignedTransactions(txs: Seq[SignedTransaction]) extends Message {
    override def code: Int = Codes.SignedTransactionsCode
    override def toShortString: String =
      s"SignedTransactions { txs: ${txs.map(_.hash.toHex)} }"
  }
}
