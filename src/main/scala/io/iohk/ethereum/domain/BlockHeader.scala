package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.{crypto, rlp}
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPList, RLPSerializable, rawDecode, encode => rlpEncode}
import BlockHeaderImplicits._
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields._
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields

/**
  * @param extraFields contains the new fields added in ECIPs 1097 and 1098 and can contain values:
  *  - HefPreECIP1098: represents the ETC blocks without checkpointing nor treasury enabled
  *  - HefPostECIP1098: represents the ETC blocks with treasury enabled but not checkpointing
  *  - HefPostECIP1097: represents the ETC blocks with both checkpointing and treasury enabled
  */
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
    nonce: ByteString,
    extraFields: HeaderExtraFields = HefEmpty
) {

  def withAdditionalExtraData(additionalBytes: ByteString): BlockHeader =
    copy(extraData = extraData ++ additionalBytes)

  def dropRightNExtraDataBytes(n: Int): BlockHeader =
    copy(extraData = extraData.dropRight(n))

  val checkpoint: Option[Checkpoint] = extraFields match {
    case HefPostEcip1097(maybeCheckpoint) => maybeCheckpoint
    case _ => None
  }

  val hasCheckpoint: Boolean = checkpoint.isDefined

  def isParentOf(child: BlockHeader): Boolean = number + 1 == child.number && child.parentHash == hash

  override def toString: String = {
    val (treasuryOptOutString: String, checkpointString: String) = extraFields match {
      case HefPostEcip1097(maybeCheckpoint) =>
        ("ECIP1098 block", maybeCheckpoint.isDefined.toString)

      case HefEmpty =>
        ("Pre-ECIP1098 block", "Pre-ECIP1097 block")
    }

    s"BlockHeader { " +
      s"hash: $hashAsHexString, " +
      s"parentHash: ${ByteStringUtils.hash2string(parentHash)}, " +
      s"ommersHash: ${ByteStringUtils.hash2string(ommersHash)}, " +
      s"beneficiary: ${ByteStringUtils.hash2string(beneficiary)} " +
      s"stateRoot: ${ByteStringUtils.hash2string(stateRoot)} " +
      s"transactionsRoot: ${ByteStringUtils.hash2string(transactionsRoot)} " +
      s"receiptsRoot: ${ByteStringUtils.hash2string(receiptsRoot)} " +
      s"logsBloom: ${ByteStringUtils.hash2string(logsBloom)} " +
      s"difficulty: $difficulty, " +
      s"number: $number, " +
      s"gasLimit: $gasLimit, " +
      s"gasUsed: $gasUsed, " +
      s"unixTimestamp: $unixTimestamp, " +
      s"extraData: ${ByteStringUtils.hash2string(extraData)} " +
      s"mixHash: ${ByteStringUtils.hash2string(mixHash)} " +
      s"nonce: ${ByteStringUtils.hash2string(nonce)}, " +
      s"treasuryOptOut: $treasuryOptOutString " +
      s"isCheckpointing: $checkpointString" +
      s"}"
  }

  /**
    * calculates blockHash for given block header
    * @return - hash that can be used to get block bodies / receipts
    */
  lazy val hash: ByteString = ByteString(kec256(this.toBytes: Array[Byte]))

  lazy val hashAsHexString: String = ByteStringUtils.hash2string(hash)

  def idTag: String =
    s"$number: $hashAsHexString"
}

object BlockHeader {

  import io.iohk.ethereum.rlp.RLPImplicits._

  /** Empty MPT root hash. Data type is irrelevant */
  val EmptyMpt: ByteString = ByteString(crypto.kec256(rlp.encode(Array.emptyByteArray)))

  val EmptyBeneficiary: ByteString = Address(0).bytes

  val EmptyOmmers: ByteString = ByteString(crypto.kec256(rlp.encode(RLPList())))

  /**
    * Given a block header, returns it's rlp encoded bytes without nonce and mix hash
    *
    * @param blockHeader to be encoded without PoW fields
    * @return rlp.encode( [blockHeader.parentHash, ..., blockHeader.extraData] + extra fields )
    */
  def getEncodedWithoutNonce(blockHeader: BlockHeader): Array[Byte] = {
    // toRLPEncodeable is guaranteed to return a RLPList
    val rlpList: RLPList = blockHeader.toRLPEncodable.asInstanceOf[RLPList]

    val numberOfPowFields = 2
    val numberOfExtraFields = blockHeader.extraFields match {
      case HefPostEcip1097(_) => 1
      case HefEmpty => 0
    }

    val preECIP1098Fields = rlpList.items.dropRight(numberOfPowFields + numberOfExtraFields)
    val extraFieldsEncoded = rlpList.items.takeRight(numberOfExtraFields)

    val rlpItemsWithoutNonce = preECIP1098Fields ++ extraFieldsEncoded
    rlpEncode(RLPList(rlpItemsWithoutNonce: _*))
  }

  sealed trait HeaderExtraFields
  object HeaderExtraFields {
    case object HefEmpty extends HeaderExtraFields
    case class HefPostEcip1097(checkpoint: Option[Checkpoint]) extends HeaderExtraFields
  }
}

object BlockHeaderImplicits {

  import io.iohk.ethereum.rlp.RLPImplicitConversions._
  import io.iohk.ethereum.rlp.RLPImplicits._

  implicit class BlockHeaderEnc(blockHeader: BlockHeader) extends RLPSerializable {
    // scalastyle:off method.length
    override def toRLPEncodable: RLPEncodeable = {
      import blockHeader._
      extraFields match {
        case HefPostEcip1097(maybeCheckpoint) =>
          RLPList(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce,
            maybeCheckpoint
          )

        case HefEmpty =>
          RLPList(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce
          )
      }
    }
  }

  implicit class BlockHeaderByteArrayDec(val bytes: Array[Byte]) extends AnyVal {
    def toBlockHeader: BlockHeader = BlockHeaderDec(rawDecode(bytes)).toBlockHeader
  }

  implicit class BlockHeaderDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {
    // scalastyle:off method.length
    def toBlockHeader: BlockHeader = {
      val checkpointOptionDecoder = implicitly[RLPDecoder[Option[Checkpoint]]]

      rlpEncodeable match {
        case RLPList(
              parentHash,
              ommersHash,
              beneficiary,
              stateRoot,
              transactionsRoot,
              receiptsRoot,
              logsBloom,
              difficulty,
              number,
              gasLimit,
              gasUsed,
              unixTimestamp,
              extraData,
              mixHash,
              nonce,
              encodedCheckpoint
            ) =>
          val extraFields = HefPostEcip1097(
            checkpointOptionDecoder.decode(encodedCheckpoint)
          )
          BlockHeader(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce,
            extraFields
          )

        case RLPList(
              parentHash,
              ommersHash,
              beneficiary,
              stateRoot,
              transactionsRoot,
              receiptsRoot,
              logsBloom,
              difficulty,
              number,
              gasLimit,
              gasUsed,
              unixTimestamp,
              extraData,
              mixHash,
              nonce
            ) =>
          BlockHeader(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce
          )

        case RLPList(
              parentHash,
              ommersHash,
              beneficiary,
              stateRoot,
              transactionsRoot,
              receiptsRoot,
              logsBloom,
              difficulty,
              number,
              gasLimit,
              gasUsed,
              unixTimestamp,
              extraData,
              mixHash,
              nonce
            ) =>
          BlockHeader(
            parentHash,
            ommersHash,
            beneficiary,
            stateRoot,
            transactionsRoot,
            receiptsRoot,
            logsBloom,
            difficulty,
            number,
            gasLimit,
            gasUsed,
            unixTimestamp,
            extraData,
            mixHash,
            nonce
          )

        case _ =>
          throw new Exception("BlockHeader cannot be decoded")
      }
    }
  }
}
