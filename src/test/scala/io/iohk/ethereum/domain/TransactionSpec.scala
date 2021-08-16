package io.iohk.ethereum.domain

import akka.util.ByteString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.SignedTransactions
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Hex
import io.iohk.ethereum.vm.utils.MockVmInput

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

  "rlp encoding then decoding transactions sequence" should "give back the initial transactions sequence" in {

    forAll(signedTxSeqGen(2, secureRandom, None)) { (originalSignedTransactionSeq: Seq[SignedTransaction]) =>
      // encode it
      import SignedTransactions.SignedTransactionsEnc

      // SignedTransactionsEnc is the Sequence version of SignedTransactionEnc
      // the SignedTransactionsEnc.toBytes calls
      // -> SignedTransactionsEnc.toRLPEncodable which maps over
      // -> SignedTransactionEnc.toRLPEncodable (single signed transaction encoder)
      // without going through the SignedTransactionEnc.toByte, which is the actual part where the 01 prefix is inserted
      val encodedSignedTransactionSeq: Array[Byte] = SignedTransactions(originalSignedTransactionSeq).toBytes

      // decode it
      import SignedTransactions.SignedTransactionsDec
      // likewise, the SignedTransactionsDec.toSignedTransactions maps over
      // -> SignedTransactionRlpEncodableDec.toSignedTransaction (single signed transaction)
      // while ignoring the SignedTransactionDec(Array[Byte]), which is responsible to parse the prefix if available
      val SignedTransactions(decodedSignedTransactionSeq) = encodedSignedTransactionSeq.toSignedTransactions

      // The test is working because both encoding and decoding are skipping the prefix part,
      // and the rlp encoded transaction is different enough to recognize a LegacyTransaction from a TX1

      // I see two problems:
      // - the encoding is not compatible with other clients
      // - this is not working for receipt, where legacy and tx1 receipt payload are the same. As such we can't
      // distinguish which one it is without the prefix

      // The root cause seems to be that the prefix stuff is done on a byte[] level,
      // whereas RLPList are working on RLPEncodable
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
  it should "correctly serialize to EIP1598 rlp" in {}

  // plan:
  // TODO ReceiptSpec
  // TODO decode string to case class
  // TODO adjust runVM in ledger
  // TODO align how geth and besu treat typed transactions when sent via rpc before magneto HF
}
