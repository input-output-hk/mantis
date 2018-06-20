package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
import io.iohk.ethereum.rlp.{RLPList, encode => rlpEncode}
import io.iohk.ethereum.utils.ToRiemann
import io.iohk.ethereum.utils.Riemann
import org.spongycastle.util.encoders.Hex

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
    nonce: ByteString) {

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
       |nonce: ${Hex.toHexString(nonce.toArray[Byte])}
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

  def getEncodedWithoutNonce(blockHeader: BlockHeader): Array[Byte] = {
    val rlpEncoded = blockHeader.toRLPEncodable match {
      case rlpList: RLPList => RLPList(rlpList.items.dropRight(2): _*)
      case _ => throw new Exception("BlockHeader cannot be encoded without nonce and mixHash")
    }
    rlpEncode(rlpEncoded)
  }

  implicit val blockHeaderToRiemann: ToRiemann[BlockHeader] =
    header => Riemann.ok("blockheader")
      .attribute("parentHash", Hex.toHexString(header.parentHash.toArray[Byte]))
      .attribute("ommersHash", Hex.toHexString(header.ommersHash.toArray[Byte]))
      .attribute("beneficiary", Hex.toHexString(header.beneficiary.toArray[Byte]))
      .attribute("stateRoot", Hex.toHexString(header.stateRoot.toArray[Byte]))
      .attribute("transactionsRoot", Hex.toHexString(header.transactionsRoot.toArray[Byte]))
      .attribute("receiptsRoot", Hex.toHexString(header.receiptsRoot.toArray[Byte]))
      .attribute("logsBloom", Hex.toHexString(header.logsBloom.toArray[Byte]))
      .attribute("difficulty", header.difficulty.toString)
      .attribute("number", header.number.toString)
      .attribute("gasLimit", header.gasLimit.toString)
      .attribute("gasUsed", header.gasUsed.toString)
      .attribute("unixTimestamp", header.unixTimestamp.toString)
      .attribute("extraData", Hex.toHexString(header.extraData.toArray[Byte]))
}
