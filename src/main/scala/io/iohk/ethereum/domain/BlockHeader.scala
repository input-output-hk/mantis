package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPSerializable, rawDecode, encode => rlpEncode}
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
    treasuryOptOut: Option[Boolean]) {

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
       |}""".stripMargin
  }

  /**
    * calculates blockHash for given block header
    * @return - hash that can be used to get block bodies / receipts
    */
  lazy val hash: ByteString = ByteString(kec256(this.toBytes: Array[Byte]))

  lazy val hashAsHexString: String = Hex.toHexString(hash.toArray)

  def idTag: String =
    s"$number: $hashAsHexString"
}

object BlockHeader {

  val emptyOmmerHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347"))

  def getEncodedWithoutNonce(blockHeader: BlockHeader): Array[Byte] = {
    val rlpEncoded = blockHeader.toRLPEncodable match {
      case rlpList: RLPList if blockHeader.treasuryOptOut.isEmpty =>
        // Pre ECIP1098 block
        RLPList(rlpList.items.dropRight(2): _*)

      case rlpList: RLPList if blockHeader.treasuryOptOut.isDefined =>
        // Post ECIP1098 block
        val rlpItemsWithoutNonce = rlpList.items.dropRight(3) :+ rlpList.items.last
        RLPList(rlpItemsWithoutNonce: _*)

      case _ => throw new Exception("BlockHeader cannot be encoded without nonce and mixHash")
    }
    rlpEncode(rlpEncoded)
  }

  implicit class BlockHeaderEnc(blockHeader: BlockHeader) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = {
      import blockHeader._
      treasuryOptOut match {
        case Some(definedOptOut) =>
          // Post ECIP1098 block, whole block is encoded
          val encodedOptOut = if(definedOptOut) 1 else 0

          RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, RLPList(encodedOptOut))

        case None =>
          // Pre ECIP1098 block, encoding works as if optOut field wasn't defined for backwards compatibility
          RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce)
      }
    }
  }

  implicit class BlockheaderDec(val bytes: Array[Byte]) extends AnyVal {
    def toBlockHeader: BlockHeader = BlockheaderEncodableDec(rawDecode(bytes)).toBlockHeader
  }

  implicit class BlockheaderEncodableDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {
    def toBlockHeader: BlockHeader = {
      rlpEncodeable match {
        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce) =>
          // Pre ECIP1098 block, encoding works as if optOut field wasn't defined for backwards compatibility
          BlockHeader(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, None)

        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, RLPList(encodedOptOut)) =>
          // Post ECIP1098 block, whole block is encoded
          val booleanOptOut =
            if ((encodedOptOut: Int) == 1) true
            else if ((encodedOptOut: Int) == 0) false
            else throw new Exception("BlockHeader cannot be decoded with an invalid opt-out")

          BlockHeader(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
            logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, Some(booleanOptOut))

        case _ =>
          throw new Exception("BlockHeader cannot be decoded")
      }
    }
  }
}
