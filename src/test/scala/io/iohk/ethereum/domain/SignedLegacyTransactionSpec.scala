package io.iohk.ethereum.domain

import akka.util.ByteString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.SignedTransaction.getSender
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.SignedTransactions
import io.iohk.ethereum.utils.Hex
import io.iohk.ethereum.vm.Generators

class SignedLegacyTransactionSpec extends AnyFlatSpec with SignedTransactionBehavior with ScalaCheckPropertyChecks {

  private def allowedPointSigns(chainId: Byte) = Set((chainId * 2 + 35).toByte, (chainId * 2 + 36).toByte)

  ("Signed LegacyTransaction" should behave).like(
    SignedTransactionBehavior(Generators.legacyTransactionGen, allowedPointSigns)
  )

  "Legacy transaction sender" should "be properly recoverable from rlp encoded values" in {

    // values are taken from https://github.com/ethereum/go-ethereum/blob/90987db7334c1d10eb866ca550efedb66dea8a20/core/types/transaction_signing_test.go#L79-L94
    val testValues = Table(
      ("binaryRLP", "expectedSender"),
      (
        "f864808504a817c800825208943535353535353535353535353535353535353535808025a0044852b2a670ade5407e78fb2863c51de9fcb96542a07186fe3aeda6bb8a116da0044852b2a670ade5407e78fb2863c51de9fcb96542a07186fe3aeda6bb8a116d",
        "0xf0f6f18bca1b28cd68e4357452947e021241e9ce"
      ),
      (
        "f864018504a817c80182a410943535353535353535353535353535353535353535018025a0489efdaa54c0f20c7adf612882df0950f5a951637e0307cdcb4c672f298b8bcaa0489efdaa54c0f20c7adf612882df0950f5a951637e0307cdcb4c672f298b8bc6",
        "0x23ef145a395ea3fa3deb533b8a9e1b4c6c25d112"
      ),
      (
        "f864028504a817c80282f618943535353535353535353535353535353535353535088025a02d7c5bef027816a800da1736444fb58a807ef4c9603b7848673f7e3a68eb14a5a02d7c5bef027816a800da1736444fb58a807ef4c9603b7848673f7e3a68eb14a5",
        "0x2e485e0c23b4c3c542628a5f672eeab0ad4888be"
      ),
      (
        "f865038504a817c803830148209435353535353535353535353535353535353535351b8025a02a80e1ef1d7842f27f2e6be0972bb708b9a135c38860dbe73c27c3486c34f4e0a02a80e1ef1d7842f27f2e6be0972bb708b9a135c38860dbe73c27c3486c34f4de",
        "0x82a88539669a3fd524d669e858935de5e5410cf0"
      ),
      (
        "f865048504a817c80483019a28943535353535353535353535353535353535353535408025a013600b294191fc92924bb3ce4b969c1e7e2bab8f4c93c3fc6d0a51733df3c063a013600b294191fc92924bb3ce4b969c1e7e2bab8f4c93c3fc6d0a51733df3c060",
        "0xf9358f2538fd5ccfeb848b64a96b743fcc930554"
      ),
      (
        "f865058504a817c8058301ec309435353535353535353535353535353535353535357d8025a04eebf77a833b30520287ddd9478ff51abbdffa30aa90a8d655dba0e8a79ce0c1a04eebf77a833b30520287ddd9478ff51abbdffa30aa90a8d655dba0e8a79ce0c1",
        "0xa8f7aba377317440bc5b26198a363ad22af1f3a4"
      ),
      (
        "f866068504a817c80683023e3894353535353535353535353535353535353535353581d88025a06455bf8ea6e7463a1046a0b52804526e119b4bf5136279614e0b1e8e296a4e2fa06455bf8ea6e7463a1046a0b52804526e119b4bf5136279614e0b1e8e296a4e2d",
        "0xf1f571dc362a0e5b2696b8e775f8491d3e50de35"
      ),
      (
        "f867078504a817c807830290409435353535353535353535353535353535353535358201578025a052f1a9b320cab38e5da8a8f97989383aab0a49165fc91c737310e4f7e9821021a052f1a9b320cab38e5da8a8f97989383aab0a49165fc91c737310e4f7e9821021",
        "0xd37922162ab7cea97c97a87551ed02c9a38b7332"
      ),
      (
        "f867088504a817c8088302e2489435353535353535353535353535353535353535358202008025a064b1702d9298fee62dfeccc57d322a463ad55ca201256d01f62b45b2e1c21c12a064b1702d9298fee62dfeccc57d322a463ad55ca201256d01f62b45b2e1c21c10",
        "0x9bddad43f934d313c2b79ca28a432dd2b7281029"
      ),
      (
        "f867098504a817c809830334509435353535353535353535353535353535353535358202d98025a052f8f61201b2b11a78d6e866abc9c3db2ae8631fa656bfe5cb53668255367afba052f8f61201b2b11a78d6e866abc9c3db2ae8631fa656bfe5cb53668255367afb",
        "0x3c24d7329e92f84f08556ceb6df1cdb0104ca49f"
      )
    )

    forAll(testValues) { (binaryRLP: String, expectedSender: String) =>
      import SignedTransactions.SignedTransactionDec
      val decodedSignedTransaction = Hex.decode(binaryRLP).toSignedTransaction

      val expectedSenderAddress = Address(expectedSender)
      val computedSenderAddressOpt = getSender(decodedSignedTransaction)
      computedSenderAddressOpt shouldEqual (Some(expectedSenderAddress))
    }
  }

  "Legacy transaction signature" should "respect EIP155 example" in {
    // values have been taken directly from the EIP-155 document
    // https://eips.ethereum.org/EIPS/eip-155
    val legacyTransaction = LegacyTransaction(
      nonce = 9,
      gasPrice = 20 * BigInt(10).pow(9),
      gasLimit = 21000,
      receivingAddress = Address("0x3535353535353535353535353535353535353535"),
      value = BigInt(10).pow(18),
      payload = ByteString.empty
    )

    val expectedSigningData =
      "ec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080"
    val expectedSigningHash = "daf5a779ae972f972197303d7b574746c7ef83eadac0f2791ad23db92e4c8e53"
    val privateKey = "4646464646464646464646464646464646464646464646464646464646464646"
    val expectedSignatureV = 37
    val expectedSignatureR = BigInt("18515461264373351373200002665853028612451056578545711640558177340181847433846")
    val expectedSignatureS = BigInt("46948507304638947509940763649030358759909902576025900602547168820602576006531")

    val expectedSignedTransaction =
      "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83"

    val signingData = SignedTransaction.bytesToSign(legacyTransaction, Some(1))

    Hex.toHexString(signingData) shouldEqual expectedSigningHash

    val senderKeyPair = crypto.keyPairFromPrvKey(Hex.decode(privateKey))
    val signedTransaction = SignedTransaction.sign(legacyTransaction, senderKeyPair, Some(1))

    signedTransaction.signature.v shouldEqual expectedSignatureV
    signedTransaction.signature.r shouldEqual expectedSignatureR
    signedTransaction.signature.s shouldEqual expectedSignatureS

    import SignedTransactions.SignedTransactionEnc
    val encodedSignedTransaction: Array[Byte] = signedTransaction.toBytes

    Hex.toHexString(encodedSignedTransaction) shouldEqual expectedSignedTransaction
  }
}
