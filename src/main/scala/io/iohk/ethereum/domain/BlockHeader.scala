package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPEncoder, RLPList, RLPSerializable, rawDecode, encode => rlpEncode}
import org.bouncycastle.util.encoders.Hex

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
    treasuryOptOut: Option[Boolean],
    checkpoint: Option[Checkpoint] = None) {

  override def toString: String = {
    s"""BlockHeader {
       |parentHash: ${Hex.toHexString(parentHash.toArray[Byte])}
       |ommersHash: ${Hex.toHexString(ommersHash.toArray[Byte])}
       |beneficiary: ${Hex.toHexString(beneficiary.toArray[Byte])}
       |stateRoot: ${Hex.toHexString(stateRoot.toArray[Byte])}
       |transactionsRoot: ${Hex.toHexString(transactionsRoot.toArray[Byte])}
       |receiptsRoot: ${Hex.toHexString(receiptsRoot.toArray[Byte])}
       |logsBloom: ${Hex.toHexString(logsBloom.toArray[Byte])}
       |difficulty: $difficulty,
       |number: $number,
       |gasLimit: $gasLimit,
       |gasUsed: $gasUsed,
       |unixTimestamp: $unixTimestamp,
       |extraData: ${Hex.toHexString(extraData.toArray[Byte])}
       |mixHash: ${Hex.toHexString(mixHash.toArray[Byte])}
       |nonce: ${Hex.toHexString(nonce.toArray[Byte])},
       |treasuryOptOut: $treasuryOptOut
       |withCheckpoint: ${checkpoint.isDefined}
       |}""".stripMargin
  }

  /**
    * calculates blockHash for given block header
    * @return - hash that can be used to get block bodies / receipts
    */
  lazy val hash: ByteString = ByteString(kec256(this.toBytes: Array[Byte]))

  lazy val hashAsHexString: String = Hex.toHexString(hash.toArray)

  val hasCheckpoint: Boolean = checkpoint.isDefined

  def idTag: String =
    s"$number: $hashAsHexString"
}

object BlockHeader {

  import Checkpoint._
  import io.iohk.ethereum.rlp.RLPImplicitConversions._
  import io.iohk.ethereum.rlp.RLPImplicits._

  private implicit val checkpointOptionDecoder = implicitly[RLPDecoder[Option[Checkpoint]]]
  private implicit val checkpointOptionEncoder = implicitly[RLPEncoder[Option[Checkpoint]]]

  val emptyOmmerHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347"))

  def getEncodedWithoutNonce(blockHeader: BlockHeader): Array[Byte] = {
    val rlpEncoded = blockHeader.toRLPEncodable match {
      case rlpList: RLPList if blockHeader.checkpoint.isDefined =>
        // post ECIP1098 & ECIP1097 block
        val rlpItemsWithoutNonce = rlpList.items.dropRight(4) ++ rlpList.items.takeRight(2)
        RLPList(rlpItemsWithoutNonce: _*)

      case rlpList: RLPList if blockHeader.treasuryOptOut.isDefined =>
        // Post ECIP1098 block without checkpoint
        val rlpItemsWithoutNonce = rlpList.items.dropRight(3) :+ rlpList.items.last
        RLPList(rlpItemsWithoutNonce: _*)

      case rlpList: RLPList if blockHeader.treasuryOptOut.isEmpty =>
        // Pre ECIP1098 & ECIP1097 block
        RLPList(rlpList.items.dropRight(2): _*)

      case _ => throw new Exception("BlockHeader cannot be encoded without nonce and mixHash")
    }
    rlpEncode(rlpEncoded)
  }

  implicit class BlockHeaderEnc(blockHeader: BlockHeader) extends RLPSerializable {
    private def encodeOptOut(definedOptOut: Boolean) = {
      val encodedOptOut = if(definedOptOut) 1 else 0
      RLPList(encodedOptOut)
    }
    override def toRLPEncodable: RLPEncodeable = {
      import blockHeader._
      (treasuryOptOut, checkpoint) match {
        case (Some(definedOptOut), Some(_)) =>
          // Post ECIP1098 & ECIP1097 block, block with treasury enabled and checkpoint is encoded
          RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, encodeOptOut(definedOptOut), checkpoint)

        case (Some(definedOptOut), None) =>
          // Post ECIP1098 block, Pre ECIP1097 or without checkpoint, block with treasury enabled is encoded
          RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, encodeOptOut(definedOptOut))

        case (None, Some(_)) =>
          // Post ECIP1097 block with checkpoint, treasury disabled, block with checkpoint is encoded
          RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, RLPList(), checkpoint)

        case _ =>
          // Pre ECIP1098 and ECIP1097 block, encoding works as if optOut and checkpoint fields weren't defined for backwards compatibility
          RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce)
      }
    }
  }

  implicit class BlockHeaderByteArrayDec(val bytes: Array[Byte]) extends AnyVal {
    def toBlockHeader: BlockHeader = BlockHeaderDec(rawDecode(bytes)).toBlockHeader
  }

  implicit class BlockHeaderDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {
    private def decodeOptOut(encodedOptOut: RLPEncodeable): Option[Boolean] = {
      val booleanOptOut = {
        if ((encodedOptOut: Int) == 1) true
        else if ((encodedOptOut: Int) == 0) false
        else throw new Exception("BlockHeader cannot be decoded with an invalid opt-out")
      }
      Some(booleanOptOut)
    }
    def toBlockHeader: BlockHeader = {
      rlpEncodeable match {
        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, RLPList(encodedOptOut), encodedCheckpoint) =>
          // Post ECIP1098 & ECIP1097 block with checkpoint, whole block is encoded
          BlockHeader(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce,
            decodeOptOut(encodedOptOut), checkpointOptionDecoder.decode(encodedCheckpoint))

        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, RLPList(), encodedCheckpoint) =>
          // Post ECIP1098 & ECIP1097 block with checkpoint and treasury disabled
          BlockHeader(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, None, checkpointOptionDecoder.decode(encodedCheckpoint))


        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, RLPList(encodedOptOut)) =>
          // Post ECIP1098 block without checkpoint
          BlockHeader(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, decodeOptOut(encodedOptOut))


        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce) =>
          // Pre ECIP1098 and ECIP1097 block, decoding works as if optOut and checkpoint fields weren't defined for backwards compatibility
          BlockHeader(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, None, None)

        case _ =>
          throw new Exception("BlockHeader cannot be decoded")
      }
    }
  }
}
