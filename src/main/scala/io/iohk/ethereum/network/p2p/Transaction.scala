package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.rlp.RLPImplicits._
import org.spongycastle.util.encoders.Hex

object Transaction {
  implicit val rlpEndDec = new RLPEncoder[Transaction] with RLPDecoder[Transaction] {
    override def encode(obj: Transaction): RLPEncodeable = {
      import obj._
      RLPList(nonce, gasPrice, gasLimit, receivingAddress.toArray[Byte], value,
        payload.fold(_.byteString.toArray[Byte], _.byteString.toArray[Byte]),
        pointSign, signatureRandom.toArray[Byte], signature.toArray[Byte])
    }

    override def decode(rlp: RLPEncodeable): Transaction = rlp match {
      case RLPList(nonce, gasPrice, gasLimit, (receivingAddress: RLPValue), value, payload, pointSign, signatureRandom, signature)
        if receivingAddress.bytes.nonEmpty =>
        Transaction(nonce, gasPrice, gasLimit, ByteString(receivingAddress: Array[Byte]), value, Right(TransactionData(ByteString(payload: Array[Byte]))),
          pointSign, ByteString(signatureRandom: Array[Byte]), ByteString(signature: Array[Byte]))

      case RLPList(nonce, gasPrice, gasLimit, (receivingAddress: RLPValue), value, payload, pointSign, signatureRandom, signature)
        if receivingAddress.bytes.isEmpty =>
        Transaction(nonce, gasPrice, gasLimit, ByteString(), value, Left(ContractInit(ByteString(payload: Array[Byte]))),
          pointSign, ByteString(signatureRandom: Array[Byte]), ByteString(signature: Array[Byte]))

      case _ => throw new RuntimeException("Cannot decode Transaction")
    }
  }
}

//ETH yellow paper section 4.3
case class Transaction(
                        nonce: BigInt,
                        gasPrice: BigInt,
                        gasLimit: BigInt,
                        receivingAddress: ByteString,
                        value: BigInt,
                        payload: Either[ContractInit, TransactionData],
                        //yellow paper appendix F
                        pointSign: Byte, //v - 27 or 28 according to yellow paper, but it is 37 and 38 in ETH
                        signatureRandom: ByteString, //r
                        signature: ByteString /*s*/) {

  override def toString: String = {
    s"""Transaction {
       |nonce: $nonce
       |gasPrice: $gasPrice
       |gasLimit: $gasLimit
       |receivingAddress: ${Hex.toHexString(receivingAddress.toArray[Byte])}
       |value: $value wei
       |payload: ${payload.fold(init => s"ContractInit [${Hex.toHexString(init.byteString.toArray[Byte])}]",
      data => s"TransactionData [${Hex.toHexString(data.byteString.toArray[Byte])}]")}
       |pointSign: $pointSign
       |signatureRandom: ${Hex.toHexString(signatureRandom.toArray[Byte])}
       |signature: ${Hex.toHexString(signature.toArray[Byte])}
       |}""".stripMargin
  }
}
