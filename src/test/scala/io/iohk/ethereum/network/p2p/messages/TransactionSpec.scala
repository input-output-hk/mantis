package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{Transaction, TransactionData}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.math.ec.ECPoint
import org.spongycastle.util.encoders.Hex

class TransactionSpec extends FlatSpec with Matchers {

  val rawPublicKey: Array[Byte] =
    Hex.decode("044c3eb5e19c71d8245eaaaba21ef8f94a70e9250848d10ade086f893a7a33a06d7063590e9e6ca88f918d7704840d903298fe802b6047fa7f6d09603eba690c39")
  val publicKey: ECPoint = crypto.curve.getCurve.decodePoint(rawPublicKey)
  val address: Array[Byte] = crypto.sha3(rawPublicKey).slice(12, 32)

  val validTransaction = Transaction(
    nonce = 172320,
    gasPrice = BigInt("50000000000"),
    gasLimit = 90000,
    receivingAddress = ByteString(Hex.decode("1c51bf013add0857c5d9cf2f71a7f15ca93d4816")),
    value = BigInt("1049756850000000000"),
    payload = Right(TransactionData(ByteString())),
    pointSign = 28,
    signatureRandom = ByteString(Hex.decode("cfe3ad31d6612f8d787c45f115cc5b43fb22bcc210b62ae71dc7cbf0a6bea8df")),
    signature = ByteString(Hex.decode("57db8998114fae3c337e99dbd8573d4085691880f4576c6c1f6c5bbfe67d6cf0")))

  val invalidTransaction: Transaction = validTransaction.copy(gasPrice = 0)

  "Transaction" should "recover sender public key" in {
    validTransaction.recoveredPublicKey.map(crypto.curve.getCurve.decodePoint) shouldBe Some(publicKey)
  }

  it should "recover sender address" in {
    validTransaction.recoveredAddress.nonEmpty shouldBe true
    validTransaction.recoveredAddress.get shouldEqual address
  }

  it should "recover false sender public key for invalid transaction" in {
    invalidTransaction.recoveredPublicKey.map(crypto.curve.getCurve.decodePoint) shouldNot be(Some(publicKey))
  }

  it should "recover false sender address for invalid transaction" in {
    invalidTransaction.recoveredAddress.nonEmpty shouldBe true
    invalidTransaction.recoveredAddress.get shouldNot equal(address)
  }
}
