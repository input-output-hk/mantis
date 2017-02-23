package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.math.ec.ECPoint
import org.spongycastle.util.encoders.Hex

class TransactionSpec extends FlatSpec with Matchers {

  val rawPublicKey: Array[Byte] =
    Hex.decode("044c3eb5e19c71d8245eaaaba21ef8f94a70e9250848d10ade086f893a7a33a06d7063590e9e6ca88f918d7704840d903298fe802b6047fa7f6d09603eba690c39")
  val publicKey: ECPoint = crypto.curve.getCurve.decodePoint(rawPublicKey)
  val address: Array[Byte] = crypto.kec256(rawPublicKey).slice(12, 32)

  val validTx = Transaction(nonce = 172320,
                            gasPrice = BigInt("50000000000"),
                            gasLimit = 90000,
                            receivingAddress = Address(Hex.decode("1c51bf013add0857c5d9cf2f71a7f15ca93d4816")),
                            value = BigInt("1049756850000000000"),
                            payload = ByteString.empty)

  val validTransactionSignatureOldSchema = SignedTransaction(
    validTx,
    pointSign = 28,
    signatureRandom = ByteString(Hex.decode("cfe3ad31d6612f8d787c45f115cc5b43fb22bcc210b62ae71dc7cbf0a6bea8df")),
    signature = ByteString(Hex.decode("57db8998114fae3c337e99dbd8573d4085691880f4576c6c1f6c5bbfe67d6cf0")))

  val invalidTransactionSignatureNewSchema = SignedTransaction(
    validTx,
    pointSign = -98,
    signatureRandom = ByteString(Hex.decode("cfe3ad31d6612f8d787c45f115cc5b43fb22bcc210b62ae71dc7cbf0a6bea8df")),
    signature = ByteString(Hex.decode("57db8998114fae3c337e99dbd8573d4085691880f4576c6c1f6c5bbfe67d6cf0")))

  val invalidStx: SignedTransaction = validTransactionSignatureOldSchema.copy(tx = validTx.copy(gasPrice = 0))

  val rawPublicKeyForNewSigningScheme: Array[Byte] =
    Hex.decode("048fc6373a74ad959fd61d10f0b35e9e0524de025cb9a2bf8e0ff60ccb3f5c5e4d566ebe3c159ad572c260719fc203d820598ee5d9c9fa8ae14ecc8d5a2d8a2af1")
  val publicKeyForNewSigningScheme: ECPoint = crypto.curve.getCurve.decodePoint(rawPublicKeyForNewSigningScheme)

  val validTransactionForNewSigningScheme = Transaction(
    nonce = 587440,
    gasPrice = BigInt("20000000000"),
    gasLimit = 90000,
    receivingAddress = Address(Hex.decode("77b95d2028c741c038735b09d8d6e99ea180d40c")),
    value = BigInt("1552986466088074000"),
    payload = ByteString.empty)

  val validSignedTransactionForNewSigningScheme = SignedTransaction(
    validTransactionForNewSigningScheme,
    pointSign = -98,
    signatureRandom = ByteString(Hex.decode("1af423b3608f3b4b35e191c26f07175331de22ed8f60d1735f03210388246ade")),
    signature = ByteString(Hex.decode("4d5b6b9e3955a0db8feec9c518d8e1aae0e1d91a143fbbca36671c3b89b89bc3")))

  "Transaction" should "recover sender public key" in {
    validTransactionSignatureOldSchema.recoveredPublicKey.map(crypto.curve.getCurve.decodePoint) shouldBe Some(publicKey)
  }

  it should "not recover sender public key for new sign encoding schema if there is no chain_id in signed data" in {
    invalidTransactionSignatureNewSchema.recoveredPublicKey.map(crypto.curve.getCurve.decodePoint) shouldNot be(Some(publicKey))
  }

  it should "recover sender public key for new sign encoding schema if there is chain_id in signed data" in {
    validSignedTransactionForNewSigningScheme.recoveredPublicKey.map(crypto.curve.getCurve.decodePoint) shouldBe Some(publicKeyForNewSigningScheme)
  }

  it should "recover sender address" in {
    validTransactionSignatureOldSchema.recoveredAddress.nonEmpty shouldBe true
    validTransactionSignatureOldSchema.recoveredAddress.get shouldEqual address
  }

  it should "recover false sender public key for invalid transaction" in {
    invalidStx.recoveredPublicKey.map(crypto.curve.getCurve.decodePoint) shouldNot be(Some(publicKey))
  }

  it should "recover false sender address for invalid transaction" in {
    invalidStx.recoveredAddress.nonEmpty shouldBe true
    invalidStx.recoveredAddress.get shouldNot equal(address)
  }

  it should "report as valid the validStx" in {
    validTransactionSignatureOldSchema.syntacticValidity shouldBe true
  }

  it should "report as invalid a tx with long nonce" in {
    val invalidNonce = (0 until Transaction.NonceLength + 1).map(_ => 1.toByte).toArray
    validTransactionSignatureOldSchema.copy(tx = validTx.copy(nonce = BigInt(invalidNonce))).syntacticValidity shouldBe false
  }

  it should "report as invalid a tx with long gas limit" in {
    val invalidGasLimit = (0 until Transaction.GasLength + 1).map(_ => 1.toByte).toArray
    validTransactionSignatureOldSchema.copy(tx = validTx.copy(gasLimit = BigInt(invalidGasLimit))).syntacticValidity shouldBe false
  }

  it should "report as invalid a tx with long gas price" in {
    val invalidGasPrice = (0 until Transaction.GasLength + 1).map(_ => 1.toByte).toArray
    validTransactionSignatureOldSchema.copy(tx = validTx.copy(gasPrice = BigInt(invalidGasPrice))).syntacticValidity shouldBe false
  }

  it should "report as invalid a tx with long value" in {
    val invalidValue = (0 until Transaction.ValueLength + 1).map(_ => 1.toByte).toArray
    validTransactionSignatureOldSchema.copy(tx = validTx.copy(value = BigInt(invalidValue))).syntacticValidity shouldBe false
  }

  it should "report as invalid a tx with long signature" in {
    val invalidSignature = (0 until ECDSASignature.SLength + 1).map(_ => 1.toByte).toArray
    validTransactionSignatureOldSchema.copy(signature = ByteString(invalidSignature)).syntacticValidity shouldBe false
  }

  it should "report as invalid a tx with long signature random" in {
    val invalidSignatureRandom = (0 until ECDSASignature.RLength + 1).map(_ => 1.toByte).toArray
    validTransactionSignatureOldSchema.copy(signatureRandom = ByteString(invalidSignatureRandom)).syntacticValidity shouldBe false
  }
}
