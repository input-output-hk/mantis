package io.iohk.ethereum.testmode

import akka.util.ByteString
import io.iohk.ethereum.consensus.{Consensus, ConsensusBuilder}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.nodebuilder.{ActorSystemBuilder, _}

import scala.concurrent.{ExecutionContext, Future}

trait TestLedgerBuilder extends LedgerBuilder {
  self: BlockchainConfigBuilder
    with BlockchainBuilder
    with SyncConfigBuilder
    with ConsensusBuilder
    with ActorSystemBuilder =>

  val executionCont = system.dispatchers.lookup("validation-context")

  lazy val testLedgerWrapper: TestLedgerWrapper =
    new TestLedgerWrapper(blockchain, syncConfig, consensus, blockchainConfig, executionCont)

  private def testLedger: Ledger = testLedgerWrapper.ledger

  class TestLedgerProxy extends Ledger {
    override def consensus: Consensus = testLedger.consensus
    override def checkBlockStatus(blockHash: ByteString): BlockStatus = testLedger.checkBlockStatus(blockHash)
    override def importBlock(block: Block)(implicit blockExecutionContext: ExecutionContext): Future[BlockImportResult] = testLedger.importBlock(block)
    override def resolveBranch(headers: Seq[BlockHeader]): BranchResolutionResult = testLedger.resolveBranch(headers)
    override def binarySearchGasEstimation(stx: SignedTransactionWithSender, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): BigInt =
      testLedger.binarySearchGasEstimation(stx, blockHeader, world)
    override def simulateTransaction(stx: SignedTransactionWithSender, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): Ledger.TxResult =
      testLedger.simulateTransaction(stx, blockHeader, world)
  }

  override lazy val ledger: Ledger = new TestLedgerProxy
}
