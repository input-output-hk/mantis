package io.iohk.ethereum.domain

import akka.util.ByteString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.SignedTransactions
import io.iohk.ethereum.utils.Hex

class TransactionSpec extends AnyFlatSpec with Matchers {
  "Transaction type 01" should "be correctly serialized to rlp" in {
    val toAddr: Address = Address.apply("b94f5374fce5edbc8e2a8697c15331677e6ebf0b")
    val tx: TransactionWithAccessList = TransactionWithAccessList(
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
    val x: Array[Byte] = stx.toBytes
    // TODO SignedTransactionEnc does not deal with bytes directly which prohibits prepending bytes directly
    // needs to be handled somehow
    val y: Array[Byte] = Hex.decode("01") ++ x
    val e =
      "01f8630103018261a894b94f5374fce5edbc8e2a8697c15331677e6ebf0b0a825544c001a0c9519f4f2b30335884581971573fadf60c6204f59a911df35ee8a540456b2660a032f1e8e2c5dd761f9e4f88f41c8310aeaba26a8bfcdacfedfa12ec3862d37521"
    val expected = Hex.decode(e)
    y shouldBe expected
  }

  it should "handle optional access list" in {}

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
      signature = sig
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
