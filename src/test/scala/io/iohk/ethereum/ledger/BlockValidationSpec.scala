package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator
import io.iohk.ethereum.domain._
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.iohk.ethereum.utils.ByteStringUtils._

class BlockValidationSpec extends AnyWordSpec with Matchers with MockFactory {

  "BlockValidation" should {
    "validate block after execution" when {
      "report valid results from execution as correct" in new BlockValidationTestSetup {
        blockValidation.validateBlockAfterExecution(block, stateRootHash, receipts, gasUsed) shouldBe Right(
          BlockExecutionSuccess
        )
      }

      "report as invalid a block that doesn't have the correct gas used" in new BlockValidationTestSetup {
        val invalidGasUsed: BigInt = gasUsed + 1
        blockValidation.validateBlockAfterExecution(block, stateRootHash, receipts, invalidGasUsed).isLeft shouldBe true
      }

      "report as invalid a block that doesn't have the correct state root hash" in new BlockValidationTestSetup {
        val invalidStateRootHash: ByteString = concatByteStrings((stateRootHash.head + 1).toByte, stateRootHash.tail)
        blockValidation.validateBlockAfterExecution(block, invalidStateRootHash, receipts, gasUsed).isLeft shouldBe true
      }

      "report as invalid a block that doesn't have the correct receipts information" in new BlockValidationTestSetup {
        val invalidReceipts: Seq[Receipt] = Seq.empty[Receipt]
        blockValidation.validateBlockAfterExecution(block, stateRootHash, invalidReceipts, gasUsed).isLeft shouldBe true
      }
    }
  }

  // scalastyle:off magic.number
  trait BlockValidationTestSetup {
    private val setup = new io.iohk.ethereum.blockchain.sync.ScenarioSetup {
      override lazy val blockchain: BlockchainImpl = mock[BlockchainImpl]

      override lazy val validators: Mocks.MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
        override val blockValidator: StdBlockValidator.type = StdBlockValidator
      }
    }

    def blockValidation: BlockValidation =
      new BlockValidation(setup.consensus, setup.blockchain, BlockQueue(setup.blockchain, setup.syncConfig))

    def hash2ByteString(hash: String): ByteString = ByteString(Hex.decode(hash))

    def mkTransaction(nonce: String, address: String, value: String): Transaction = Transaction(
      nonce = BigInt(nonce),
      gasPrice = BigInt("20000000000"),
      gasLimit = BigInt("50000"),
      receivingAddress = Address(hash2ByteString(address)),
      value = BigInt(value),
      payload = ByteString.empty
    )

    def mkStx(tx: Transaction, random: String, signature: String): SignedTransaction = SignedTransaction(
      tx = tx,
      pointSign = 0x9d.toByte,
      signatureRandom = hash2ByteString(random),
      signature = hash2ByteString(signature),
      chainId = 0x3d.toByte
    )

    val bloomFilter: ByteString = hash2ByteString("0" * 512)

    val block: Block = Block(
      BlockHeader(
        parentHash = hash2ByteString("8345d132564b3660aa5f27c9415310634b50dbc92579c65a0825d9a255227a71"),
        ommersHash = hash2ByteString("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347"),
        beneficiary = hash2ByteString("df7d7e053933b5cc24372f878c90e62dadad5d42"),
        stateRoot = hash2ByteString("087f96537eba43885ab563227262580b27fc5e6516db79a6fc4d3bcd241dda67"),
        transactionsRoot = hash2ByteString("8ae451039a8bf403b899dcd23252d94761ddd23b88c769d9b7996546edc47fac"),
        receiptsRoot = hash2ByteString("8b472d8d4d39bae6a5570c2a42276ed2d6a56ac51a1a356d5b17c5564d01fd5d"),
        logsBloom = bloomFilter,
        difficulty = BigInt("14005986920576"),
        number = 3125369,
        gasLimit = 4699996,
        gasUsed = 84000,
        unixTimestamp = 1486131165,
        extraData = hash2ByteString("d5830104098650617269747986312e31332e30826c69"),
        mixHash = hash2ByteString("be90ac33b3f6d0316e60eef505ff5ec7333c9f3c85c1a36fc2523cd6b75ddb8a"),
        nonce = hash2ByteString("2b0fb0c002946392")
      ),
      BlockBody(
        transactionList = Seq[SignedTransaction](
          mkStx(
            mkTransaction("438550", "ee4439beb5c71513b080bbf9393441697a29f478", "1265230129703017984"),
            "5b496e526a65eac3c4312e683361bfdb873741acd3714c3bf1bcd7f01dd57ccb",
            "3a30af5f529c7fc1d43cfed773275290475337c5e499f383afd012edcc8d7299"
          ),
          mkStx(
            mkTransaction("438551", "c68e9954c7422f479e344faace70c692217ea05b", "656010196207162880"),
            "377e542cd9cd0a4414752a18d0862a5d6ced24ee6dba26b583cd85bc435b0ccf",
            "579fee4fd96ecf9a92ec450be3c9a139a687aa3c72c7e43cfac8c1feaf65c4ac"
          ),
          mkStx(
            mkTransaction("438552", "19c5a95eeae4446c5d24363eab4355157e4f828b", "3725976610361427456"),
            "a70267341ba0b33f7e6f122080aa767d52ba4879776b793c35efec31dc70778d",
            "3f66ed7f0197627cbedfe80fd8e525e8bc6c5519aae7955e7493591dcdf1d6d2"
          ),
          mkStx(
            mkTransaction("438553", "3435be928d783b7c48a2c3109cba0d97d680747a", "108516826677274384"),
            "beb8226bdb90216ca29967871a6663b56bdd7b86cf3788796b52fd1ea3606698",
            "2446994156bc1780cb5806e730b171b38307d5de5b9b0d9ad1f9de82e00316b5"
          )
        ),
        uncleNodesList = Seq.empty[BlockHeader]
      )
    )

    def mkReceipt(stateHash: String, gas: BigInt): Receipt = Receipt.withHashOutcome(
      postTransactionStateHash = hash2ByteString(stateHash),
      cumulativeGasUsed = gas,
      logsBloomFilter = bloomFilter,
      logs = Seq.empty[TxLogEntry]
    )

    val receipts: Seq[Receipt] = Seq(
      mkReceipt("ce0ac687bb90d457b6573d74e4a25ea7c012fee329eb386dbef161c847f9842d", 21000),
      mkReceipt("b927d361126302acaa1fa5e93d0b7e349e278231fe2fc2846bfd54f50377f20a", 42000),
      mkReceipt("1e913d6bdd412d71292173d7908f8792adcf958b84c89575bc871a1decaee56d", 63000),
      mkReceipt("0c6e052bc83482bafaccffc4217adad49f3a9533c69c820966d75ed0154091e6", 84000)
    )

    val stateRootHash: ByteString = block.header.stateRoot
    val gasUsed: BigInt = block.header.gasUsed

  }
}
