package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.BlockchainConfig
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.math.ec.ECPoint
import org.spongycastle.util.encoders.Hex

class TransactionSpec extends FlatSpec with Matchers {

  val blockchainConfig = BlockchainConfig(Config.config)

  val rawPublicKey: Array[Byte] =
    Hex.decode("044c3eb5e19c71d8245eaaaba21ef8f94a70e9250848d10ade086f893a7a33a06d7063590e9e6ca88f918d7704840d903298fe802b6047fa7f6d09603eba690c39")
  val publicKey: ECPoint = crypto.curve.getCurve.decodePoint(rawPublicKey)
  val address: Address = Address(crypto.kec256(rawPublicKey.tail).slice(12, 32))

  val validTx = Transaction(nonce = 172320,
                            gasPrice = BigInt("50000000000"),
                            gasLimit = 90000,
                            receivingAddress = Address(Hex.decode("1c51bf013add0857c5d9cf2f71a7f15ca93d4816")),
                            value = BigInt("1049756850000000000"),
                            payload = ByteString.empty)

  val validTransactionSignatureOldSchema = SignedTransaction(
    validTx,
    pointSign = 28.toByte,
    signatureRandom = ByteString(Hex.decode("cfe3ad31d6612f8d787c45f115cc5b43fb22bcc210b62ae71dc7cbf0a6bea8df")),
    signature = ByteString(Hex.decode("57db8998114fae3c337e99dbd8573d4085691880f4576c6c1f6c5bbfe67d6cf0")),
    chainId = blockchainConfig.chainId)

  val invalidTransactionSignatureNewSchema = SignedTransaction(
    validTx,
    pointSign = (-98).toByte,
    signatureRandom = ByteString(Hex.decode("cfe3ad31d6612f8d787c45f115cc5b43fb22bcc210b62ae71dc7cbf0a6bea8df")),
    signature = ByteString(Hex.decode("57db8998114fae3c337e99dbd8573d4085691880f4576c6c1f6c5bbfe67d6cf0")),
    chainId = blockchainConfig.chainId)

  val invalidStx = SignedTransaction(
    validTx.copy(gasPrice = 0),
    pointSign = (-98).toByte,
    signatureRandom = ByteString(Hex.decode("cfe3ad31d6612f8d787c45f115cc5b43fb22bcc210b62ae71dc7cbf0a6bea8df")),
    signature = ByteString(Hex.decode("57db8998114fae3c337e99dbd8573d4085691880f4576c6c1f6c5bbfe67d6cf0")),
    chainId = blockchainConfig.chainId)

  val rawPublicKeyForNewSigningScheme: Array[Byte] =
    Hex.decode("048fc6373a74ad959fd61d10f0b35e9e0524de025cb9a2bf8e0ff60ccb3f5c5e4d566ebe3c159ad572c260719fc203d820598ee5d9c9fa8ae14ecc8d5a2d8a2af1")
  val publicKeyForNewSigningScheme: ECPoint = crypto.curve.getCurve.decodePoint(rawPublicKeyForNewSigningScheme)
  val addreesForNewSigningScheme = Address(crypto.kec256(rawPublicKeyForNewSigningScheme.tail).slice(12, 32))

  val validTransactionForNewSigningScheme = Transaction(
    nonce = 587440,
    gasPrice = BigInt("20000000000"),
    gasLimit = 90000,
    receivingAddress = Address(Hex.decode("77b95d2028c741c038735b09d8d6e99ea180d40c")),
    value = BigInt("1552986466088074000"),
    payload = ByteString.empty)

  val validSignedTransactionForNewSigningScheme = SignedTransaction(
    tx = validTransactionForNewSigningScheme,
    pointSign = (-98).toByte,
    signatureRandom = ByteString(Hex.decode("1af423b3608f3b4b35e191c26f07175331de22ed8f60d1735f03210388246ade")),
    signature = ByteString(Hex.decode("4d5b6b9e3955a0db8feec9c518d8e1aae0e1d91a143fbbca36671c3b89b89bc3")),
    blockchainConfig.chainId)

  val stxWithInvalidPointSign = SignedTransaction(
    validTx,
    pointSign = 26.toByte,
    signatureRandom = ByteString(Hex.decode("cfe3ad31d6612f8d787c45f115cc5b43fb22bcc210b62ae71dc7cbf0a6bea8df")),
    signature = ByteString(Hex.decode("57db8998114fae3c337e99dbd8573d4085691880f4576c6c1f6c5bbfe67d6cf0")),
    blockchainConfig.chainId)

  it should "not recover sender public key for new sign encoding schema if there is no chain_id in signed data" in {
    invalidTransactionSignatureNewSchema.map(_.senderAddress) shouldNot be(Some(address))
  }

  it should "recover sender address" in {
    validTransactionSignatureOldSchema.map(_.senderAddress) shouldEqual Some(address)
  }

  it should "recover sender for new sign encoding schema if there is chain_id in signed data" in {
    validSignedTransactionForNewSigningScheme.map(_.senderAddress) shouldBe Some(addreesForNewSigningScheme)
  }

  it should "recover false sender address for invalid transaction" in {
    invalidStx.map(_.senderAddress) shouldNot be(Some(address))
  }

  it should "not recover a sender address for transaction with invalid point sign" in {
    stxWithInvalidPointSign.map(_.senderAddress) shouldBe None
  }

  it should "recover the correct sender for tx in block 46147" in {
    val stx: SignedTransaction = SignedTransaction(
      tx = Transaction(
        nonce = BigInt(0),
        gasPrice = BigInt("50000000000000"),
        gasLimit = BigInt(21000),
        receivingAddress = Address(ByteString(Hex.decode("5df9b87991262f6ba471f09758cde1c0fc1de734"))),
        value = BigInt(31337),
        payload = ByteString.empty
      ),
      pointSign = 28.toByte,
      signatureRandom = ByteString(BigInt("61965845294689009770156372156374760022787886965323743865986648153755601564112").toByteArray),
      signature = ByteString(BigInt("31606574786494953692291101914709926755545765281581808821704454381804773090106").toByteArray),
      chainId = blockchainConfig.chainId
    ).get

    stx.senderAddress shouldBe  Address(ByteString(Hex.decode("a1e4380a3b1f749673e270229993ee55f35663b4")))
  }


}
