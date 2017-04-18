package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class ValidationsAfterExecutionSpec extends FlatSpec with Matchers {

  val block: Block = Block(
    BlockHeader(
      parentHash = ByteString(Hex.decode("8345d132564b3660aa5f27c9415310634b50dbc92579c65a0825d9a255227a71")),
      ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
      beneficiary = ByteString(Hex.decode("df7d7e053933b5cc24372f878c90e62dadad5d42")),
      stateRoot = ByteString(Hex.decode("087f96537eba43885ab563227262580b27fc5e6516db79a6fc4d3bcd241dda67")),
      transactionsRoot = ByteString(Hex.decode("8ae451039a8bf403b899dcd23252d94761ddd23b88c769d9b7996546edc47fac")),
      receiptsRoot = ByteString(Hex.decode("8b472d8d4d39bae6a5570c2a42276ed2d6a56ac51a1a356d5b17c5564d01fd5d")),
      logsBloom = ByteString(Hex.decode("0" * 512)),
      difficulty = BigInt("14005986920576"),
      number = 3125369,
      gasLimit = 4699996,
      gasUsed = 84000,
      unixTimestamp = 1486131165,
      extraData = ByteString(Hex.decode("d5830104098650617269747986312e31332e30826c69")),
      mixHash = ByteString(Hex.decode("be90ac33b3f6d0316e60eef505ff5ec7333c9f3c85c1a36fc2523cd6b75ddb8a")),
      nonce = ByteString(Hex.decode("2b0fb0c002946392"))
    ),
    BlockBody(
      transactionList = Seq[SignedTransaction](
        SignedTransaction(
          tx = Transaction(
            nonce = BigInt("438550"),
            gasPrice = BigInt("20000000000"),
            gasLimit = BigInt("50000"),
            receivingAddress = Address(ByteString(Hex.decode("ee4439beb5c71513b080bbf9393441697a29f478"))),
            value = BigInt("1265230129703017984"),
            payload = ByteString.empty
          ),
          pointSign = 0x9d.toByte,
          signatureRandom = ByteString(Hex.decode("5b496e526a65eac3c4312e683361bfdb873741acd3714c3bf1bcd7f01dd57ccb")),
          signature = ByteString(Hex.decode("3a30af5f529c7fc1d43cfed773275290475337c5e499f383afd012edcc8d7299")),
          chainId = 0x3d.toByte
        ).get, SignedTransaction(
          tx = Transaction(
            nonce = BigInt("438551"),
            gasPrice = BigInt("20000000000"),
            gasLimit = BigInt("50000"),
            receivingAddress = Address(ByteString(Hex.decode("c68e9954c7422f479e344faace70c692217ea05b"))),
            value = BigInt("656010196207162880"),
            payload = ByteString.empty
          ),
          pointSign = 0x9d.toByte,
          signatureRandom = ByteString(Hex.decode("377e542cd9cd0a4414752a18d0862a5d6ced24ee6dba26b583cd85bc435b0ccf")),
          signature = ByteString(Hex.decode("579fee4fd96ecf9a92ec450be3c9a139a687aa3c72c7e43cfac8c1feaf65c4ac")),
          chainId = 0x3d.toByte
        ).get, SignedTransaction(
          tx = Transaction(
            nonce = BigInt("438552"),
            gasPrice = BigInt("20000000000"),
            gasLimit = BigInt("50000"),
            receivingAddress = Address(ByteString(Hex.decode("19c5a95eeae4446c5d24363eab4355157e4f828b"))),
            value = BigInt("3725976610361427456"),
            payload = ByteString.empty
          ),
          pointSign = 0x9d.toByte,
          signatureRandom = ByteString(Hex.decode("a70267341ba0b33f7e6f122080aa767d52ba4879776b793c35efec31dc70778d")),
          signature = ByteString(Hex.decode("3f66ed7f0197627cbedfe80fd8e525e8bc6c5519aae7955e7493591dcdf1d6d2")),
          chainId = 0x3d.toByte
        ).get, SignedTransaction(
          tx = Transaction(
            nonce = BigInt("438553"),
            gasPrice = BigInt("20000000000"),
            gasLimit = BigInt("50000"),
            receivingAddress = Address(ByteString(Hex.decode("3435be928d783b7c48a2c3109cba0d97d680747a"))),
            value = BigInt("108516826677274384"),
            payload = ByteString.empty
          ),
          pointSign = 0x9d.toByte,
          signatureRandom = ByteString(Hex.decode("beb8226bdb90216ca29967871a6663b56bdd7b86cf3788796b52fd1ea3606698")),
          signature = ByteString(Hex.decode("2446994156bc1780cb5806e730b171b38307d5de5b9b0d9ad1f9de82e00316b5")),
          chainId = 0x3d.toByte
        ).get
      ),
      uncleNodesList = Seq[BlockHeader]()
    )
  )
  val receipts: Seq[Receipt] = Seq(
    Receipt(
      postTransactionStateHash = ByteString(Hex.decode("ce0ac687bb90d457b6573d74e4a25ea7c012fee329eb386dbef161c847f9842d")),
      cumulativeGasUsed = 21000,
      logsBloomFilter = ByteString(Hex.decode("0" * 512)),
      logs = Seq[TxLogEntry]()
    ),
    Receipt(
      postTransactionStateHash = ByteString(Hex.decode("b927d361126302acaa1fa5e93d0b7e349e278231fe2fc2846bfd54f50377f20a")),
      cumulativeGasUsed = 42000,
      logsBloomFilter = ByteString(Hex.decode("0" * 512)),
      logs = Seq[TxLogEntry]()
    ),
    Receipt(
      postTransactionStateHash = ByteString(Hex.decode("1e913d6bdd412d71292173d7908f8792adcf958b84c89575bc871a1decaee56d")),
      cumulativeGasUsed = 63000,
      logsBloomFilter = ByteString(Hex.decode("0" * 512)),
      logs = Seq[TxLogEntry]()
    ),
    Receipt(
      postTransactionStateHash = ByteString(Hex.decode("0c6e052bc83482bafaccffc4217adad49f3a9533c69c820966d75ed0154091e6")),
      cumulativeGasUsed = 84000,
      logsBloomFilter = ByteString(Hex.decode("0" * 512)),
      logs = Seq[TxLogEntry]()
    )
  )
  val stateRootHash = block.header.stateRoot
  val gasUsed = block.header.gasUsed

  it should "report valid results from execution as correct" in {
    Ledger.validateBlockAfterExecution(block, stateRootHash, receipts, gasUsed) shouldBe Right(())
  }

  it should "report as invalid a block that doesn't have the correct gas used" in {
    val invalidGasUsed = gasUsed + 1
    assert(Ledger.validateBlockAfterExecution(block, stateRootHash, receipts, invalidGasUsed).isLeft)
  }

  it should "report as invalid a block that doesn't have the correct state root hash" in {
    val invalidStateRootHash: ByteString = (stateRootHash.head + 1).toByte +: stateRootHash.tail
    assert(Ledger.validateBlockAfterExecution(block, invalidStateRootHash, receipts, gasUsed).isLeft)
  }

  it should "report as invalid a block that doesn't have the correct receipts information" in {
    val invalidReceipts: Seq[Receipt] = Seq()
    assert(Ledger.validateBlockAfterExecution(block, stateRootHash, invalidReceipts, gasUsed).isLeft)
  }

}
