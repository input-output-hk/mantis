package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator.OmmersError.OmmersNotValidError
import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator.OmmersValid
import io.iohk.ethereum.consensus.ethash.validators.{EthashValidators, OmmersValidator}
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderNumberError
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.{BlockTransactionsHashError, BlockValid}
import io.iohk.ethereum.consensus.{Consensus, GetBlockHeaderByHash, GetNBlocksBack}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{StateBeforeFailure, TxsExecutionError}
import io.iohk.ethereum.ledger.Ledger.BlockPreparationResult
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.{ConnectedState, DisconnectedState, Handshaker, HandshakerState}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.vm._

object Mocks {

  class MockLedger(blockchain: BlockchainImpl, shouldExecuteCorrectly: (Block, BlockchainImpl) => Boolean) extends Ledger{
    def consensus: Consensus = ??? // FIXME Implement

    override def checkBlockStatus(blockHash:ByteString): BlockStatus = ??? // FIXME Implement

    override def executeBlock(block: Block, alreadyValidated: Boolean = false)
    : Either[BlockExecutionError, Seq[Receipt]] = {
      if(shouldExecuteCorrectly(block, blockchain))
        Right(Nil)
      else
        Left(TxsExecutionError(Fixtures.Blocks.Block3125369.body.transactionList.head,
          StateBeforeFailure(blockchain.getWorldStateProxy(0, UInt256.Zero),0,Nil),
          "StubLedger was set to fail for this case"))
    }

    override def prepareBlock(block: Block): BlockPreparationResult = {
      // FIXME Implement
      ???
    }

    override def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): Ledger.TxResult = {
      // FIXME Implement
      ???
    }

    def importBlock(block: Block): BlockImportResult = ???

    def resolveBranch(headers: Seq[BlockHeader]): BranchResolutionResult = ???

    def binarySearchGasEstimation(stx: SignedTransaction, blockHeader: BlockHeader, world: Option[InMemoryWorldStateProxy]): BigInt = ???

  }

  private val defaultProgramResult: Ledger.PC => Ledger.PR = context => ProgramResult(
    returnData = ByteString.empty,
    gasRemaining = 1000000 - 25000,
    world = context.world,
    addressesToDelete = Set.empty,
    logs = Nil,
    internalTxs = Nil,
    gasRefund = 20000,
    error = None
  )

  class MockVM(runFn: Ledger.PC => Ledger.PR = defaultProgramResult) extends VM {
    override def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] =
      runFn(context.asInstanceOf[Ledger.PC]).asInstanceOf[ProgramResult[W, S]]
  }

  class MockValidatorsFailingOnBlockBodies extends MockValidatorsAlwaysSucceed {

    override val blockValidator: BlockValidator = new BlockValidator {
      override def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]) = Right(BlockValid)
      override def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Left(BlockTransactionsHashError)
    }
  }

  class MockValidatorsAlwaysSucceed extends EthashValidators {

    override val blockValidator: BlockValidator = new BlockValidator {
      override def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]) = Right(BlockValid)
      override def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Right(BlockValid)
    }

    override val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidator {
      def validate(blockHeader: BlockHeader, getBlockHeaderByHash: ByteString => Option[BlockHeader]): Either[BlockHeaderError, BlockHeaderValid] =
        Right(BlockHeaderValid)
    }

    override val ommersValidator: OmmersValidator =
      (parentHash: ByteString,
        blockNumber: BigInt,
        ommers: Seq[BlockHeader],
        getBlockHeaderByHash: GetBlockHeaderByHash,
        getNBlocksBack: GetNBlocksBack) => Right(OmmersValid)

    override val signedTransactionValidator: SignedTransactionValidator =
      (stx: SignedTransaction, account: Account, blockHeader: BlockHeader, upfrontGasCost: UInt256, accumGasLimit: BigInt) => Right(SignedTransactionValid)
  }

  object MockValidatorsAlwaysSucceed extends MockValidatorsAlwaysSucceed

  object MockValidatorsAlwaysFail extends EthashValidators {
    override val signedTransactionValidator = new SignedTransactionValidator {
      def validate(stx: SignedTransaction, account: Account, blockHeader: BlockHeader,
                   upfrontGasCost: UInt256, accumGasLimit: BigInt) = Left(SignedTransactionError.TransactionSignatureError)
    }

    override val blockHeaderValidator = new BlockHeaderValidator {
      def validate(blockHeader: BlockHeader, getBlockHeaderByHash: ByteString => Option[BlockHeader]) = Left(HeaderNumberError)
    }

    override val ommersValidator: OmmersValidator =
      (parentHash: ByteString,
        blockNumber: BigInt,
        ommers: Seq[BlockHeader],
        getBlockHeaderByHash: GetBlockHeaderByHash,
        getNBlocksBack: GetNBlocksBack) => Left(OmmersNotValidError)

    override val blockValidator = new BlockValidator {
      def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Left(BlockTransactionsHashError)
      def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]) = Left(BlockTransactionsHashError)
    }
  }

  case class MockHandshakerAlwaysSucceeds(initialStatus: Status, currentMaxBlockNumber: BigInt,
                                          forkAccepted: Boolean) extends Handshaker[PeerInfo] {
    override val handshakerState: HandshakerState[PeerInfo] =
      ConnectedState(PeerInfo(initialStatus, initialStatus.totalDifficulty, forkAccepted, currentMaxBlockNumber))
    override def copy(handshakerState: HandshakerState[PeerInfo]): Handshaker[PeerInfo] = this
  }

  case class MockHandshakerAlwaysFails(reason: Int) extends Handshaker[PeerInfo] {
    override val handshakerState: HandshakerState[PeerInfo] = DisconnectedState(reason)

    override def copy(handshakerState: HandshakerState[PeerInfo]): Handshaker[PeerInfo] = this
  }

}
