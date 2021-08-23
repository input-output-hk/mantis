package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex

import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.MessageSerializableImplicit
import io.iohk.ethereum.rlp.RLPCodec.Ops
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicitDerivations.RLPListDecoder
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.utils.ByteStringUtils.ByteStringOps
import io.iohk.ethereum.utils.Config

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
          val address = rlpAddress.decodeAs[Address]("address")
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
      import TypedTransaction._

      def toNewBlock: NewBlock = rawDecode(bytes) match {
        case RLPList(RLPList(blockHeader, transactionList: RLPList, uncleNodesList: RLPList), totalDifficulty) =>
          NewBlock(
            Block(
              blockHeader.toBlockHeader,
              BlockBody(
                transactionList.items.toTypedRLPEncodables.map(_.toSignedTransaction),
                uncleNodesList.items.map(_.toBlockHeader)
              )
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

  object TypedTransaction {
    implicit class TypedTransactionsRLPAggregator(val encodables: Seq[RLPEncodeable]) extends AnyVal {

      import Transaction.ByteArrayTransactionTypeValidator

      /** Convert a Seq of RLPEncodable containing TypedTransaction informations into a Seq of
        * Prefixed RLPEncodable.
        *
        * PrefixedRLPEncodable(prefix, prefixedRLPEncodable) generates binary data
        * as prefix || RLPEncodable(prefixedRLPEncodable).
        *
        * As prefix is a byte value lower than 0x7f, it is read back as RLPValue(prefix),
        * thus PrefixedRLPEncodable is binary equivalent to RLPValue(prefix), RLPEncodable
        *
        * The method aggregates back the typed transaction prefix with the following heuristic:
        * - a RLPValue(byte) with byte < 07f + the following RLPEncodable are associated as a PrefixedRLPEncodable
        * - all other RLPEncodable are kept unchanged
        *
        * This is the responsibility of the RLPDecoder to insert this meaning into its RLPList, when appropriate.
        *
        * @return a Seq of TypedTransaction enriched RLPEncodable
        */
      def toTypedRLPEncodables: Seq[RLPEncodeable] =
        encodables match {
          case Seq() => Seq()
          case Seq(RLPValue(v), rlpList: RLPList, tail @ _*) if v.isValidTransactionType =>
            PrefixedRLPEncodable(v.head, rlpList) +: tail.toTypedRLPEncodables
          case Seq(head, tail @ _*) => head +: tail.toTypedRLPEncodables
        }
    }
  }

  object SignedTransactions {

    implicit class SignedTransactionEnc(val signedTx: SignedTransaction) extends RLPSerializable {

      override def toRLPEncodable: RLPEncodeable = {
        val receivingAddressBytes = signedTx.tx.receivingAddress
          .map(_.toArray)
          .getOrElse(Array.emptyByteArray)
        signedTx.tx match {
          case TransactionWithAccessList(chainId, nonce, gasPrice, gasLimit, _, value, payload, accessList) =>
            PrefixedRLPEncodable(
              Transaction.Type01,
              RLPList(
                chainId,
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

      import TypedTransaction._

      def toSignedTransactions: SignedTransactions = rawDecode(bytes) match {
        case rlpList: RLPList => SignedTransactions(rlpList.items.toTypedRLPEncodables.map(_.toSignedTransaction))
        case _                => throw new RuntimeException("Cannot decode SignedTransactions")
      }
    }

    implicit class SignedTransactionRlpEncodableDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {

      // scalastyle:off method.length
      /** A signed transaction is either a RLPList representing a Legacy SignedTransaction
        * or a PrefixedRLPEncodable(transactionType, RLPList of typed transaction envelope)
        *
        * @see TypedTransaction.TypedTransactionsRLPAggregator
        *
        * @return a SignedTransaction
        */
      def toSignedTransaction: SignedTransaction = rlpEncodeable match {
        case PrefixedRLPEncodable(
              Transaction.Type01,
              RLPList(
                chainId,
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
              )
            ) =>
          val receivingAddressOpt = if (receivingAddress.bytes.isEmpty) None else Some(Address(receivingAddress.bytes))
          SignedTransaction(
            TransactionWithAccessList(
              chainId,
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
    // scalastyle:on method.length

    implicit class SignedTransactionDec(val bytes: Array[Byte]) extends AnyVal {
      def toSignedTransaction: SignedTransaction = {
        val first = bytes(0)
        (first match {
          case Transaction.Type01 => PrefixedRLPEncodable(Transaction.Type01, rawDecode(bytes.tail))
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
