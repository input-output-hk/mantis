package io.iohk.ethereum.domain

import akka.util.ByteString
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
