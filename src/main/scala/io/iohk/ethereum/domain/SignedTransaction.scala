package io.iohk.ethereum.domain

import java.math.BigInteger

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}
import io.iohk.ethereum.rlp.RLPImplicits._
import org.spongycastle.util.encoders.Hex


object SignedTransaction {

  val FirstByteOfAddress = 12
  val AddressLength = 20
  val LastByteOfAddress = FirstByteOfAddress + AddressLength

}

case class SignedTransaction(
  tx: Transaction,
  pointSign: Byte, //v - 27 or 28 according to yellow paper, but it is 37 and 38 in ETH
  signatureRandom: ByteString, //r
  signature: ByteString /*s*/) {

  import SignedTransaction._

  lazy val bytesToSign: Array[Byte] =
    crypto.sha3(
      rlpEncode(RLPList(
                  tx.nonce,
                  tx.gasPrice,
                  tx.gasLimit,
                  tx.receivingAddress.toArray,
                  tx.value,
                  tx.payload)))


  lazy val recoveredAddress: Option[Array[Byte]] =
    recoveredPublicKey.map(key => crypto.sha3(key).slice(FirstByteOfAddress, LastByteOfAddress))

  lazy val recoveredPublicKey: Option[Array[Byte]] =
    ECDSASignature.recoverPubBytes(
      new BigInteger(1, signatureRandom.toArray[Byte]),
      new BigInteger(1, signature.toArray[Byte]),
      ECDSASignature.recIdFromSignatureV(pointSign),
      bytesToSign)

  lazy val syntacticValidity: Boolean = {

    import tx._
    import Transaction._

    def byteLength(b: BigInt): Int = b.toByteArray.length

    byteLength(nonce) <= NonceLength &&
    (receivingAddress.bytes.isEmpty || receivingAddress.bytes.length == AddressLength) &&
    byteLength(gasLimit) <= GasLength &&
    byteLength(gasPrice) <= GasLength &&
    byteLength(value) <= ValueLength &&
    signatureRandom.length <= ECDSASignature.RLength &&
    signature.length <= ECDSASignature.SLength &&
    recoveredAddress.isDefined && recoveredAddress.get.length == AddressLength
  }

  override def toString: String = {
    s"""SignedTransaction {
         |tx: $tx
         |pointSign: $pointSign
         |signatureRandom: ${Hex.toHexString(signatureRandom.toArray[Byte])}
         |signature: ${Hex.toHexString(signatureRandom.toArray[Byte])}
         |bytesToSign: ${Hex.toHexString(bytesToSign)}
         |recoveredPublicKey: ${recoveredPublicKey.map(Hex.toHexString)}
         |recoveredAddress: ${recoveredAddress.map(Hex.toHexString)}
         |}""".stripMargin
  }
}
