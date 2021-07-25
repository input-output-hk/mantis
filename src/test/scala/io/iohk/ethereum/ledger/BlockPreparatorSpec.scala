package io.iohk.ethereum.ledger

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor4
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.Mocks
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.consensus.mining.Mining
import io.iohk.ethereum.consensus.validators.SignedTransactionError
import io.iohk.ethereum.consensus.validators.SignedTransactionError.TransactionSignatureError
import io.iohk.ethereum.consensus.validators.SignedTransactionValid
import io.iohk.ethereum.consensus.validators.SignedTransactionValidator
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockResult
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.InvalidJump
import io.iohk.ethereum.vm.InvalidOpCode
import io.iohk.ethereum.vm.OutOfGas
import io.iohk.ethereum.vm.ProgramError
import io.iohk.ethereum.vm.RevertOccurs
import io.iohk.ethereum.vm.StackOverflow
import io.iohk.ethereum.vm.StackUnderflow

// scalastyle:off magic.number
class BlockPreparatorSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  "BlockPreparator" should {

    "correctly change the nonce" when {
      "executing a tx that results in contract creation" in new TestSetup {

        val tx: LegacyTransaction =
          defaultTx.copy(
            gasPrice = defaultGasPrice,
            gasLimit = defaultGasLimit,
            receivingAddress = None,
            payload = ByteString.empty
          )

        val stx: SignedTransactionWithSender = SignedTransactionWithSender(
          SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId)),
          Address(originKeyPair)
        )

        val header: BlockHeader = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

        val postTxWorld: InMemoryWorldStateProxy =
          mining.blockPreparator
            .executeTransaction(stx.tx, stx.senderAddress, header, worldWithMinerAndOriginAccounts)
            .worldState

        postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
      }

      "executing a tx that results in a message call" in new TestSetup {

        val tx: LegacyTransaction = defaultTx.copy(
          gasPrice = defaultGasPrice,
          gasLimit = defaultGasLimit,
          receivingAddress = Some(originAddress),
          payload = ByteString.empty
        )

        val stx: SignedTransactionWithSender = SignedTransactionWithSender(
          SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId)),
          Address(originKeyPair)
        )

        val header: BlockHeader = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

        val postTxWorld: InMemoryWorldStateProxy =
          mining.blockPreparator
            .executeTransaction(stx.tx, stx.senderAddress, header, worldWithMinerAndOriginAccounts)
            .worldState

        postTxWorld.getGuaranteedAccount(originAddress).nonce shouldBe (initialOriginNonce + 1)
      }
    }

    "properly assign stateRootHash" when {
      "before byzantium block (exclusive)" in new TestSetup {

        val tx: LegacyTransaction = defaultTx.copy(
          gasPrice = defaultGasPrice,
          gasLimit = defaultGasLimit,
          receivingAddress = None,
          payload = ByteString.empty
        )
        val stx: SignedTransactionWithSender = SignedTransactionWithSender(
          SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId)),
          Address(originKeyPair)
        )
        val header: BlockHeader =
          defaultBlockHeader.copy(number = blockchainConfig.forkBlockNumbers.byzantiumBlockNumber - 1)

        val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
          mining.blockPreparator.executeTransactions(Seq(stx.tx), initialWorld, header)

        result shouldBe a[Right[_, BlockResult]]
        result.map { br =>
          br.receipts.last.postTransactionStateHash shouldBe a[HashOutcome]
        }
      }

      "after byzantium block (inclusive) if operation is a success" in new TestSetup {

        val tx: LegacyTransaction = defaultTx.copy(
          gasPrice = defaultGasPrice,
          gasLimit = defaultGasLimit,
          receivingAddress = None,
          payload = ByteString.empty
        )
        val stx: SignedTransaction = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))
        val header: BlockHeader =
          defaultBlockHeader.copy(
            beneficiary = minerAddress.bytes,
            number = blockchainConfig.forkBlockNumbers.byzantiumBlockNumber
          )

        val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
          mining.blockPreparator.executeTransactions(Seq(stx), initialWorld, header)

        result shouldBe a[Right[_, BlockResult]]
        result.map(_.receipts.last.postTransactionStateHash shouldBe SuccessOutcome)
      }

      "after byzantium block (inclusive) if operation is a failure" in new TestSetup {

        val defaultsLogs = Seq(defaultLog)

        lazy val mockVM =
          new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, Some(RevertOccurs), bEmpty, defaultsLogs))

        val testMining: Mining = newTestMining(vm = mockVM)

        val tx: LegacyTransaction = defaultTx.copy(
          gasPrice = defaultGasLimit,
          gasLimit = defaultGasLimit,
          receivingAddress = None,
          payload = ByteString.empty
        )
        val stx: SignedTransactionWithSender = SignedTransactionWithSender(
          SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId)),
          Address(originKeyPair)
        )
        val header: BlockHeader =
          defaultBlockHeader.copy(
            beneficiary = minerAddress.bytes,
            number = blockchainConfig.forkBlockNumbers.byzantiumBlockNumber
          )

        val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
          testMining.blockPreparator.executeTransactions(Seq(stx.tx), initialWorld, header)

        result shouldBe a[Right[_, BlockResult]]
        result.map(_.receipts.last.postTransactionStateHash shouldBe FailureOutcome)
      }
    }

    "correctly calculate the total gas refund to be returned to the sender and paying for gas to the miner" in new TestSetup {

      val table: TableFor4[BigInt, BigInt, Option[ProgramError], BigInt] =
        Table[BigInt, BigInt, Option[ProgramError], BigInt](
          ("execGasUsed", "refundsFromVM", "maybeError", "gasUsed"),
          (25000, 20000, None, 25000 - 12500),
          (25000, 10000, None, 25000 - 10000),
          (125000, 10000, Some(OutOfGas), defaultGasLimit),
          (125000, 100000, Some(OutOfGas), defaultGasLimit),
          (125000, 100000, Some(RevertOccurs), 125000)
        )

      forAll(table) { (execGasUsed, gasRefundFromVM, error, gasUsed) =>
        val balanceDelta = UInt256(gasUsed * defaultGasPrice)

        val tx = defaultTx.copy(gasPrice = defaultGasPrice, gasLimit = defaultGasLimit)

        val stx = SignedTransactionWithSender(
          SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId)),
          Address(originKeyPair)
        )

        val header = defaultBlockHeader.copy(beneficiary = minerAddress.bytes)

        val mockVM = new MockVM(c =>
          createResult(
            context = c,
            gasUsed = execGasUsed,
            gasLimit = defaultGasLimit,
            gasRefund = gasRefundFromVM,
            error = error
          )
        )

        val execResult = mining
          .withVM(mockVM)
          .blockPreparator
          .executeTransaction(stx.tx, stx.senderAddress, header, worldWithMinerAndOriginAccounts)

        val postTxWorld = execResult.worldState

        execResult.gasUsed shouldEqual gasUsed
        postTxWorld.getBalance(originAddress) shouldEqual (initialOriginBalance - balanceDelta)
        postTxWorld.getBalance(minerAddress) shouldEqual (initialMinerBalance + balanceDelta)
      }
    }
  }

  "clear logs only if vm execution results in an error" in new TestSetup {

    val defaultsLogs = Seq(defaultLog)

    val table: TableFor2[Option[ProgramError], Int] = Table[Option[ProgramError], Int](
      ("Execution Error", "Logs size"),
      (Some(InvalidOpCode(1)), 0),
      (Some(OutOfGas), 0),
      (Some(InvalidJump(23)), 0),
      (Some(StackOverflow), 0),
      (Some(StackUnderflow), 0),
      (None, defaultsLogs.size)
    )

    forAll(table) { (maybeError, logsSize) =>
      val initialOriginBalance: UInt256 = 1000000

      val initialOriginNonce = defaultTx.nonce

      val initialWorld = emptyWorld
        .saveAccount(originAddress, Account(nonce = UInt256(initialOriginNonce), balance = initialOriginBalance))

      val stx = SignedTransactionWithSender(
        SignedTransaction.sign(defaultTx, originKeyPair, Some(blockchainConfig.chainId)),
        Address(originKeyPair)
      )

      val mockVM = new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, maybeError, bEmpty, defaultsLogs))

      val testMining = newTestMining(vm = mockVM)

      val txResult =
        testMining.blockPreparator.executeTransaction(stx.tx, stx.senderAddress, defaultBlockHeader, initialWorld)

      txResult.logs.size shouldBe logsSize
    }
  }

  "create sender account if it does not exists" in new TestSetup {

    val inputData = ByteString("the payload")

    val newAccountKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    val newAccountAddress =
      Address(kec256(newAccountKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    override lazy val vm: VMImpl = new MockVM((pc: PC) =>
      createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString("contract code"))
    )

    val tx: LegacyTransaction = defaultTx.copy(gasPrice = 0, receivingAddress = None, payload = inputData)
    val stx = SignedTransaction.sign(tx, newAccountKeyPair, Some(blockchainConfig.chainId))

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      mining.blockPreparator.executeTransactions(
        Seq(stx),
        initialWorld,
        defaultBlockHeader
      )

    result shouldBe a[Right[_, BlockResult]]
    result.map(br => br.worldState.getAccount(newAccountAddress)) shouldBe Right(Some(Account(nonce = 1)))
  }

  "remember executed transaction in case of many failures in the middle" in new TestSetup {
    val newAccountKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    Address(kec256(newAccountKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    override lazy val vm: VMImpl =
      new MockVM((pc: PC) => createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString.empty))

    override lazy val validators: MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
      override val signedTransactionValidator: SignedTransactionValidator =
        new SignedTransactionValidator {
          def validate(
              stx: SignedTransaction,
              senderAccount: Account,
              blockHeader: BlockHeader,
              upfrontGasCost: UInt256,
              accumGasUsed: BigInt
          )(implicit blockchainConfig: BlockchainConfig): Either[SignedTransactionError, SignedTransactionValid] =
            if (stx.tx.receivingAddress.contains(Address(42))) {
              Right(SignedTransactionValid)
            } else {
              Left(TransactionSignatureError)
            }
        }
    }

    val tx1: LegacyTransaction = defaultTx.copy(gasPrice = 42, receivingAddress = Some(Address(42)))
    val tx2: LegacyTransaction = defaultTx.copy(gasPrice = 43, receivingAddress = Some(Address(43)))
    val tx3: LegacyTransaction = defaultTx.copy(gasPrice = 43, receivingAddress = Some(Address(43)))
    val tx4: LegacyTransaction = defaultTx.copy(gasPrice = 42, receivingAddress = Some(Address(42)))
    val stx1 = SignedTransaction.sign(tx1, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx2 = SignedTransaction.sign(tx2, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx3 = SignedTransaction.sign(tx3, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx4 = SignedTransaction.sign(tx4, newAccountKeyPair, Some(blockchainConfig.chainId))

    val result: (BlockResult, Seq[SignedTransaction]) = mining.blockPreparator.executePreparedTransactions(
      Seq(stx1, stx2, stx3, stx4),
      initialWorld,
      defaultBlockHeader
    )

    result match { case (_, executedTxs) => executedTxs shouldBe Seq(stx1, stx4) }
  }

  "produce empty block if all txs fail" in new TestSetup {
    val newAccountKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    Address(kec256(newAccountKeyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    override lazy val vm =
      new MockVM((pc: PC) => createResult(pc, defaultGasLimit, defaultGasLimit, 0, None, returnData = ByteString.empty))

    override lazy val validators: Mocks.MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
      override val signedTransactionValidator: SignedTransactionValidator =
        new SignedTransactionValidator {
          def validate(
              stx: SignedTransaction,
              senderAccount: Account,
              blockHeader: BlockHeader,
              upfrontGasCost: UInt256,
              accumGasUsed: BigInt
          )(implicit blockchainConfig: BlockchainConfig): Either[SignedTransactionError, SignedTransactionValid] =
            Left(TransactionSignatureError)
        }
    }

    val tx1: LegacyTransaction = defaultTx.copy(gasPrice = 42, receivingAddress = Some(Address(42)))
    val tx2: LegacyTransaction = defaultTx.copy(gasPrice = 42, receivingAddress = Some(Address(42)))
    val stx1 = SignedTransaction.sign(tx1, newAccountKeyPair, Some(blockchainConfig.chainId))
    val stx2 = SignedTransaction.sign(tx2, newAccountKeyPair, Some(blockchainConfig.chainId))

    val result: (BlockResult, Seq[SignedTransaction]) =
      mining.blockPreparator.executePreparedTransactions(Seq(stx1, stx2), initialWorld, defaultBlockHeader)

    result match { case (_, executedTxs) => executedTxs shouldBe Seq.empty }
  }

  // migrated from old LedgerSpec
  "properly assign stateRootHash before byzantium block (exclusive)" in new TestSetup {

    val tx: LegacyTransaction = defaultTx.copy(
      gasPrice = defaultGasPrice,
      gasLimit = defaultGasLimit,
      receivingAddress = None,
      payload = ByteString.empty
    )
    val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))
    val header: BlockHeader =
      defaultBlockHeader.copy(number = blockchainConfig.forkBlockNumbers.byzantiumBlockNumber - 1)

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      mining.blockPreparator.executeTransactions(Seq(stx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map { br =>
      br.receipts.last.postTransactionStateHash shouldBe a[HashOutcome]
    }
  }

  "properly assign stateRootHash after byzantium block (inclusive) if operation is a success" in new TestSetup {

    val tx: LegacyTransaction = defaultTx.copy(
      gasPrice = defaultGasPrice,
      gasLimit = defaultGasLimit,
      receivingAddress = None,
      payload = ByteString.empty
    )
    val stx: SignedTransaction = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))
    val header: BlockHeader =
      defaultBlockHeader.copy(
        beneficiary = minerAddress.bytes,
        number = blockchainConfig.forkBlockNumbers.byzantiumBlockNumber
      )

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      mining.blockPreparator.executeTransactions(Seq(stx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map(_.receipts.last.postTransactionStateHash shouldBe SuccessOutcome)
  }

  "properly assign stateRootHash after byzantium block (inclusive) if operation is a failure" in new TestSetup {

    val defaultsLogs = Seq(defaultLog)

    lazy val mockVM =
      new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, Some(RevertOccurs), bEmpty, defaultsLogs))

    val testMining: Mining = newTestMining(vm = mockVM)

    val tx: LegacyTransaction = defaultTx.copy(
      gasPrice = defaultGasLimit,
      gasLimit = defaultGasLimit,
      receivingAddress = None,
      payload = ByteString.empty
    )
    val stx = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))
    val header: BlockHeader =
      defaultBlockHeader.copy(
        beneficiary = minerAddress.bytes,
        number = blockchainConfig.forkBlockNumbers.byzantiumBlockNumber
      )

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      testMining.blockPreparator.executeTransactions(Seq(stx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map(_.receipts.last.postTransactionStateHash shouldBe FailureOutcome)
  }
}
