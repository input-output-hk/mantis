package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPList, RLPSerializable, rawDecode, encode => rlpEncode}
import org.bouncycastle.util.encoders.Hex
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
    extraFields: HeaderExtraFields) {

  val treasuryOptOut: Option[Boolean] = extraFields match {
    case HefPostEcip1097(definedOptOut, _) => Some(definedOptOut)
    case HefPostEcip1098(definedOptOut) => Some(definedOptOut)
    case HefPreEcip1098 => None
  }

  override def toString: String = {
    val (treasuryOptOutString: String, checkpointString: String) = extraFields match {
      case HefPostEcip1097(definedOptOut, maybeCheckpoint) =>
        (definedOptOut.toString, maybeCheckpoint.isDefined.toString)

      case HefPostEcip1098(definedOptOut) =>
        (definedOptOut.toString, "Pre-ECIP1097 block")

      case HefPreEcip1098 =>
        ("Pre-ECIP1098 block", "Pre-ECIP1097 block")
    }

    s"""BlockHeader {
       |hash: $hashAsHexString
       |parentHash: ${ByteStringUtils.hash2string(parentHash)}
       |ommersHash: ${ByteStringUtils.hash2string(ommersHash)}
       |beneficiary: ${ByteStringUtils.hash2string(beneficiary)}
       |stateRoot: ${ByteStringUtils.hash2string(stateRoot)}
       |transactionsRoot: ${ByteStringUtils.hash2string(transactionsRoot)}
       |receiptsRoot: ${ByteStringUtils.hash2string(receiptsRoot)}
       |logsBloom: ${ByteStringUtils.hash2string(logsBloom)}
       |difficulty: $difficulty,
       |number: $number,
       |gasLimit: $gasLimit,
       |gasUsed: $gasUsed,
       |unixTimestamp: $unixTimestamp,
       |extraData: ${ByteStringUtils.hash2string(extraData)}
       |mixHash: ${ByteStringUtils.hash2string(mixHash)}
       |nonce: ${ByteStringUtils.hash2string(nonce)},
       |treasuryOptOut: $treasuryOptOutString
       |isCheckpointing: $checkpointString
       |}""".stripMargin
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

  val emptyOmmerHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347"))

  def getEncodedWithoutNonce(blockHeader: BlockHeader): Array[Byte] = {
    // toRLPEncodeable is guaranteed to return a RLPList
    val rlpList: RLPList = blockHeader.toRLPEncodable.asInstanceOf[RLPList]

    val rlpItemsWithoutNonce = blockHeader.extraFields match {
      case HefPostEcip1097(_ , _) =>
        // Post ECIP1098 & ECIP1097 block
        rlpList.items.dropRight(4) ++ rlpList.items.takeRight(2)

      case HefPostEcip1098(_) =>
        // Post ECIP1098 block, Pre ECIP1097
        rlpList.items.dropRight(3) :+ rlpList.items.last

      case HefPreEcip1098 =>
        // Pre ECIP1098 and ECIP1097 block, encoding works as if optOut and checkpoint fields weren't defined for backwards compatibility
        rlpList.items.dropRight(2)
    }
    rlpEncode(RLPList(rlpItemsWithoutNonce: _*))
  }

  // scalastyle:off parameter.number
  def buildPreECIP1098Header(parentHash: ByteString,
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
                             nonce: ByteString): BlockHeader = BlockHeader(
    parentHash = parentHash,
    ommersHash = ommersHash,
    beneficiary = beneficiary,
    stateRoot = stateRoot,
    transactionsRoot = transactionsRoot,
    receiptsRoot = receiptsRoot,
    logsBloom = logsBloom,
    difficulty = difficulty,
    number = number,
    gasLimit = gasLimit,
    gasUsed = gasUsed,
    unixTimestamp = unixTimestamp,
    extraData = extraData,
    mixHash = mixHash,
    nonce = nonce,
    extraFields = HefPreEcip1098
  )

  def buildPostECIP1098Header(parentHash: ByteString,
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
                              treasuryOptOut: Boolean): BlockHeader = BlockHeader(
    parentHash = parentHash,
    ommersHash = ommersHash,
    beneficiary = beneficiary,
    stateRoot = stateRoot,
    transactionsRoot = transactionsRoot,
    receiptsRoot = receiptsRoot,
    logsBloom = logsBloom,
    difficulty = difficulty,
    number = number,
    gasLimit = gasLimit,
    gasUsed = gasUsed,
    unixTimestamp = unixTimestamp,
    extraData = extraData,
    mixHash = mixHash,
    nonce = nonce,
    extraFields = HefPostEcip1098(treasuryOptOut)
  )

  def buildPostECIP1097Header(parentHash: ByteString,
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
                              treasuryOptOut: Boolean,
                              checkpoint: Option[Checkpoint]): BlockHeader = BlockHeader(
    parentHash = parentHash,
    ommersHash = ommersHash,
    beneficiary = beneficiary,
    stateRoot = stateRoot,
    transactionsRoot = transactionsRoot,
    receiptsRoot = receiptsRoot,
    logsBloom = logsBloom,
    difficulty = difficulty,
    number = number,
    gasLimit = gasLimit,
    gasUsed = gasUsed,
    unixTimestamp = unixTimestamp,
    extraData = extraData,
    mixHash = mixHash,
    nonce = nonce,
    extraFields = HefPostEcip1097(treasuryOptOut, checkpoint)
  )
  // scalastyle:on parameter.number

  sealed trait HeaderExtraFields
  object HeaderExtraFields {
    case object HefPreEcip1098 extends HeaderExtraFields
    case class HefPostEcip1098(treasuryOptOut: Boolean) extends HeaderExtraFields
    case class HefPostEcip1097(treasuryOptOut: Boolean, checkpoint: Option[Checkpoint]) extends HeaderExtraFields
  }
}

object BlockHeaderImplicits {

  import io.iohk.ethereum.rlp.RLPImplicitConversions._
  import io.iohk.ethereum.rlp.RLPImplicits._

  implicit class BlockHeaderEnc(blockHeader: BlockHeader) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = {
      import blockHeader._
      extraFields match {
        case HefPostEcip1097(definedOptOut, maybeCheckpoint) =>
          RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, definedOptOut, maybeCheckpoint)

        case HefPostEcip1098(definedOptOut) =>
          RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, definedOptOut)

        case HefPreEcip1098 =>
          RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce)
      }
    }
  }

  implicit class BlockHeaderByteArrayDec(val bytes: Array[Byte]) extends AnyVal {
    def toBlockHeader: BlockHeader = BlockHeaderDec(rawDecode(bytes)).toBlockHeader
  }

  implicit class BlockHeaderDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {
    def toBlockHeader: BlockHeader = {
      val checkpointOptionDecoder = implicitly[RLPDecoder[Option[Checkpoint]]]
      val treasuryOptOutDecoder = implicitly[RLPDecoder[Boolean]]

      rlpEncodeable match {
        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, encodedOptOut, encodedCheckpoint) =>
          BlockHeader.buildPostECIP1097Header(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce,
            treasuryOptOutDecoder.decode(encodedOptOut), checkpointOptionDecoder.decode(encodedCheckpoint))

        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, encodedOptOut) =>
          BlockHeader.buildPostECIP1098Header(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce,
            treasuryOptOutDecoder.decode(encodedOptOut))

        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce) =>
          BlockHeader.buildPreECIP1098Header(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce)

        case _ =>
          throw new Exception("BlockHeader cannot be decoded")
      }
    }
  }
}
