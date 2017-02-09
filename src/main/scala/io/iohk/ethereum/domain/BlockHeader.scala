package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto.sha3
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
import io.iohk.ethereum.rlp
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
  }

object BlockHeader {
    /**
      * calculates blockHash for given block header
      * @param header - block header for which hash will be calculated
      * @return - hash that can be used to get block bodies / receipts
      */
    def hash(header: BlockHeader): ByteString = {
        ByteString(sha3(rlp.encode[BlockHeader](header)))
    }
}
