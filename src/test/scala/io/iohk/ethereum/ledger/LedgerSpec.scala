package io.iohk.ethereum.ledger

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.{BlockTransactionsHashError, BlockValid}
import io.iohk.ethereum.consensus.validators.{Validators, _}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.BlockResult
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.ledger.BlockRewardCalculatorOps._
import io.iohk.ethereum.vm.{OutOfGas, RevertOccurs}
import monix.execution.Scheduler
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.util.encoders.Hex
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.{TableFor2, TableFor3, TableFor4}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.iohk.ethereum.utils.ByteStringUtils._

// scalastyle:off magic.number
class LedgerSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers with ScalaFutures {

  implicit val testContext: Scheduler = Scheduler.fixedPool("ledger-spec", 4)

  it should "properly find minimal required gas limit to execute transaction" in new BinarySimulationChopSetup {
    testGasValues.foreach { minimumRequiredGas =>
      LedgerUtils.binaryChop[TxError](minimalGas, maximalGas)(
        mockTransaction(minimumRequiredGas)
      ) shouldEqual minimumRequiredGas
    }
  }

  it should "properly assign stateRootHash before byzantium block (exclusive)" in new TestSetup {

    val tx: Transaction = defaultTx.copy(
      gasPrice = defaultGasPrice,
      gasLimit = defaultGasLimit,
      receivingAddress = None,
      payload = ByteString.empty
    )
    val stx: SignedTransactionWithSender = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))
    val header: BlockHeader =
      defaultBlockHeader.copy(number = blockchainConfig.forkBlockNumbers.byzantiumBlockNumber - 1)

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      consensus.blockPreparator.executeTransactions(Seq(stx.tx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map { br =>
      br.receipts.last.postTransactionStateHash shouldBe a[HashOutcome]
    }
  }

  it should "properly assign stateRootHash after byzantium block (inclusive) if operation is a success" in new TestSetup {

    val tx: Transaction = defaultTx.copy(
      gasPrice = defaultGasPrice,
      gasLimit = defaultGasLimit,
      receivingAddress = None,
      payload = ByteString.empty
    )
    val stx: SignedTransaction = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId)).tx
    val header: BlockHeader =
      defaultBlockHeader.copy(
        beneficiary = minerAddress.bytes,
        number = blockchainConfig.forkBlockNumbers.byzantiumBlockNumber
      )

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      consensus.blockPreparator.executeTransactions(Seq(stx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map(_.receipts.last.postTransactionStateHash shouldBe SuccessOutcome)
  }

  it should "properly assign stateRootHash after byzantium block (inclusive) if operation is a failure" in new TestSetup {

    val defaultsLogs = Seq(defaultLog)

    lazy val mockVM =
      new MockVM(createResult(_, defaultGasLimit, defaultGasLimit, 0, Some(RevertOccurs), bEmpty, defaultsLogs))

    val testConsensus: Consensus = newTestConsensus(vm = mockVM)

    val tx: Transaction = defaultTx.copy(
      gasPrice = defaultGasLimit,
      gasLimit = defaultGasLimit,
      receivingAddress = None,
      payload = ByteString.empty
    )
    val stx: SignedTransactionWithSender = SignedTransaction.sign(tx, originKeyPair, Some(blockchainConfig.chainId))
    val header: BlockHeader =
      defaultBlockHeader.copy(
        beneficiary = minerAddress.bytes,
        number = blockchainConfig.forkBlockNumbers.byzantiumBlockNumber
      )

    val result: Either[BlockExecutionError.TxsExecutionError, BlockResult] =
      testConsensus.blockPreparator.executeTransactions(Seq(stx.tx), initialWorld, header)

    result shouldBe a[Right[_, BlockResult]]
    result.map(_.receipts.last.postTransactionStateHash shouldBe FailureOutcome)
  }
}
