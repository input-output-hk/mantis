package io.iohk.ethereum.testmode

import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.consensus.{Consensus, ConsensusBuilder}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.nodebuilder.{ActorSystemBuilder, _}
import monix.eval.Task
import monix.execution.Scheduler

trait TestLedgerBuilder extends LedgerBuilder {
  self: BlockchainConfigBuilder
    with BlockchainBuilder
    with SyncConfigBuilder
    with ConsensusBuilder
    with ActorSystemBuilder =>

  val scheduler = Scheduler(system.dispatchers.lookup("validation-context"))

  lazy val testLedgerWrapper: TestLedgerWrapper =
    new TestLedgerWrapper(blockchain, syncConfig, consensus, blockchainConfig, scheduler)

  private def testLedger: Ledger = testLedgerWrapper.ledger

  class TestLedgerProxy extends Ledger {
    override def consensus: Consensus = testLedger.consensus
    override def checkBlockStatus(blockHash: ByteString): BlockStatus = testLedger.checkBlockStatus(blockHash)
    override def getBlockByHash(hash: ByteString): Option[Block] = testLedger.getBlockByHash(hash)
    override def importBlock(block: Block)(implicit
        blockExecutionScheduler: Scheduler
    ): Task[BlockImportResult] = testLedger.importBlock(block)
    override def resolveBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult =
      testLedger.resolveBranch(headers)
  }

  override lazy val ledger: Ledger = new TestLedgerProxy
  override lazy val stxLedger: StxLedger = testLedgerWrapper.stxLedger
}
