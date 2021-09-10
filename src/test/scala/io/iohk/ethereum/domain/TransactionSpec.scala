package io.iohk.ethereum.domain

import akka.util.ByteString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.crypto.pubKeyFromKeyPair
import io.iohk.ethereum.domain.SignedTransaction.getSender
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.SignedTransactions
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.Hex

class TransactionSpec
    extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with ObjectGenerators
    with SecureRandomBuilder
    with Matchers {

  "rlp encoding then decoding transaction" should "give back the initial transaction" in {

    forAll(signedTxGen(secureRandom, None)) { (originalSignedTransaction: SignedTransaction) =>
      // encode it
      import SignedTransactions.SignedTransactionEnc
      val encodedSignedTransaction: Array[Byte] = originalSignedTransaction.toBytes

      // decode it
      import SignedTransactions.SignedTransactionDec
      val decodedSignedTransaction = encodedSignedTransaction.toSignedTransaction

      decodedSignedTransaction shouldEqual originalSignedTransaction
    }
  }

  "signing transaction, encoding and decoding it" should "allow to retrieve the proper sender" in {

    forAll(transactionGen) { (originalTransaction: Transaction) =>
      val senderKeys = crypto.generateKeyPair(secureRandom)

      val originalSenderAddress = {
        // You get a public address for your account by taking the last 20 bytes of the Keccak-256 hash of the public key and adding 0x to the beginning.
        ECDSASignature
        val pubKey = pubKeyFromKeyPair(senderKeys)
        val hashedPublickKey = kec256(pubKey)
        val slice = hashedPublickKey.slice(hashedPublickKey.length - 20, hashedPublickKey.length)
        Address(slice)
      }

      val originalSignedTransaction = SignedTransaction.sign(originalTransaction, senderKeys, Some(1))
      // check for proper signature content
      getSender(originalSignedTransaction) shouldEqual (Some(originalSenderAddress))

      // encode it
      import SignedTransactions.SignedTransactionEnc
      val encodedSignedTransaction: Array[Byte] = originalSignedTransaction.toBytes

      // decode it
      import SignedTransactions.SignedTransactionDec
      val decodedSignedTransaction = encodedSignedTransaction.toSignedTransaction

      // resolve original sender
      getSender(originalSignedTransaction)
      getSender(decodedSignedTransaction) shouldEqual getSender(originalSignedTransaction)
    }
  }

  "rlp encoding then decoding transactions sequence" should "give back the initial transactions sequence" in {

    forAll(signedTxSeqGen(2, secureRandom, None)) { (originalSignedTransactionSeq: Seq[SignedTransaction]) =>
      // encode it
      import SignedTransactions.SignedTransactionsEnc
      val encodedSignedTransactionSeq: Array[Byte] = SignedTransactions(originalSignedTransactionSeq).toBytes

      // decode it
      import SignedTransactions.SignedTransactionsDec
      val SignedTransactions(decodedSignedTransactionSeq) = encodedSignedTransactionSeq.toSignedTransactions

      decodedSignedTransactionSeq shouldEqual originalSignedTransactionSeq
    }
  }

  "Transaction type 01" should "be correctly serialized to rlp" in {

    // binary values have be taken directly from core-geth own tests
    // see https://github.com/ethereum/go-ethereum/blob/a580f7d6c54812ef47df94c6ffc974c9dbc48245/core/types/transaction_test.go#L71

    val toAddr: Address = Address.apply("b94f5374fce5edbc8e2a8697c15331677e6ebf0b")
    val tx: TransactionWithAccessList = TransactionWithAccessList(
      1, // ethereum mainnet, used by the core-geth test
      3,
      1,
      25000,
      toAddr,
      10,
      ByteString(Hex.decode("5544")),
      Nil
    )
    val stx = SignedTransaction.apply(
      tx = tx,
      ECDSASignature
        .fromBytes(
          ByteString(
            Hex.decode(
              "c9519f4f2b30335884581971573fadf60c6204f59a911df35ee8a540456b266032f1e8e2c5dd761f9e4f88f41c8310aeaba26a8bfcdacfedfa12ec3862d3752101"
            )
          )
        )
        .get
    )
    import SignedTransactions.SignedTransactionEnc
    val encodedSignedTransaction: Array[Byte] = stx.toBytes
    val e =
      "01f8630103018261a894b94f5374fce5edbc8e2a8697c15331677e6ebf0b0a825544c001a0c9519f4f2b30335884581971573fadf60c6204f59a911df35ee8a540456b2660a032f1e8e2c5dd761f9e4f88f41c8310aeaba26a8bfcdacfedfa12ec3862d37521"
    val expected = Hex.decode(e)
    encodedSignedTransaction shouldBe expected
  }

  "Legacy transaction" should "correctly serialize to original rlp" in {
    val toAddr: Address = Address.apply("b94f5374fce5edbc8e2a8697c15331677e6ebf0b")
    val tx: LegacyTransaction = LegacyTransaction(
      3,
      1,
      2000,
      toAddr,
      10,
      ByteString(Hex.decode("5544"))
    )
    val sig = ECDSASignature
      .fromBytes(
        ByteString(
          Hex.decode(
            "98ff921201554726367d2be8c804a7ff89ccf285ebc57dff8ae4c44b9c19ac4a8887321be575c8095f789dd4c743dfe42c1820f9231f98a962b210e3ac2452a301"
          )
        )
      )
      .get
    val stx = SignedTransaction.apply(
      tx = tx,
      // hacky change to make the test succeed without regressing the general workflow.
      // Mantis is currently importing *raw* signature values, and doesn't changes them
      // when building a signed transaction from a signature and a transaction.
      // On the other side, core-geth is updating the signature field v depending on the type
      // of transaction and the expected signature rule (homestead, eip155 or eip2930 for example).
      // Mantis lacks this feature. Until the signers feature is integrated, we'll keep this localised
      // hack to check for legacy transaction regression.
      // The 27 magic number is taken from the yellow paper and eip155, which stipulate that
      // transaction.v = signature.yParity (here ECDSA.v raw field) + 27
      // This should be fixed in ETCM-1096
      signature = sig.copy(v = (sig.v + 27).toByte)
    )

    import SignedTransactions.SignedTransactionEnc
    val x: Array[Byte] = stx.toBytes
    val e =
      "f86103018207d094b94f5374fce5edbc8e2a8697c15331677e6ebf0b0a8255441ca098ff921201554726367d2be8c804a7ff89ccf285ebc57dff8ae4c44b9c19ac4aa08887321be575c8095f789dd4c743dfe42c1820f9231f98a962b210e3ac2452a3"

    val expected = Hex.decode(e)
    x shouldBe expected

  }
}
