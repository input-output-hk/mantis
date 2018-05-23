package io.iohk.ethereum.testmode

import akka.util.ByteString
import io.iohk.ethereum.consensus.{Consensus, ConsensusBuilder}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.nodebuilder.{BlockchainBuilder, BlockchainConfigBuilder, LedgerBuilder, SyncConfigBuilder}

trait TestLedgerBuilder extends LedgerBuilder {
  self: BlockchainConfigBuilder
    with BlockchainBuilder
    with SyncConfigBuilder
    with ConsensusBuilder =>

  lazy val testLedgerWrapper: TestLedgerWrapper = new TestLedgerWrapper(blockchain, syncConfig, consensus, blockchainConfig)

  private def testLedger: Ledger = testLedgerWrapper.ledger

  class TestLedgerProxy extends Ledger {
    override def consensus: Consensus = testLedger.consensus
    override def checkBlockStatus(blockHash: ByteString): BlockStatus = testLedger.checkBlockStatus(blockHash)
    override def prepareBlock(block: Block): Ledger.BlockPreparationResult = testLedger.prepareBlock(block)
    override def importBlock(block: Block): BlockImportResult = testLedger.importBlock(block)
    override def resolveBranch(headers: Seq[BlockHeader]): BranchResolutionResult = testLedger.resolveBranch(headers)
    override def executeBlock(block: Block, alreadyValidated: Boolean): Either[BlockExecutionError, Seq[Receipt]] =
      testLedger.executeBlock(block, alreadyValidated)
    override def binarySearchGasEstimation(stx: SignedTransaction, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): BigInt =
      testLedger.binarySearchGasEstimation(stx, blockHeader, world)
    override def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): Ledger.TxResult =
      testLedger.simulateTransaction(stx, blockHeader, world)
  }

  override lazy val ledger: Ledger = new TestLedgerProxy
}
