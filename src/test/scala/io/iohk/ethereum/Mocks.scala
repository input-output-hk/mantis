package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.TxsExecutionError
import io.iohk.ethereum.ledger.Ledger.BlockPreparationResult
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.handshaker.{ConnectedState, DisconnectedState, Handshaker, HandshakerState}
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockPreparationError, Ledger}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.validators.BlockHeaderError.HeaderNumberError
import io.iohk.ethereum.validators.BlockValidator.BlockTransactionsHashError
import io.iohk.ethereum.validators.OmmersValidator.OmmersError.OmmersNotValidError
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm._

object Mocks {

  class MockLedger(shouldExecuteCorrectly: (Block, BlockchainStorages, Validators) => Boolean) extends Ledger{
    override def executeBlock(block: Block, storages: BlockchainStorages, validators: Validators)
    : Either[BlockExecutionError, Seq[Receipt]] = {
      if(shouldExecuteCorrectly(block, storages, validators))
        Right(Nil)
      else
        Left(TxsExecutionError(Fixtures.Blocks.Block3125369.body.transactionList.head, "StubLedger was set to fail for this case"))
    }

    override def prepareBlock(block: Block, storages: BlockchainStorages, validators: Validators):
    Either[BlockPreparationError, BlockPreparationResult] = {
      ???
    }

    override def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader, storages: BlockchainStorages): Ledger.TxResult = {
      ???
    }
  }

  private val defaultProgramResult: Ledger.PC => Ledger.PR = context => ProgramResult(
    returnData = ByteString.empty,
    gasRemaining = 1000000 - 25000,
    world = context.world,
    addressesToDelete = Set.empty,
    logs = Nil,
    gasRefund = 20000,
    error = None
  )

  class MockVM(runFn: Ledger.PC => Ledger.PR = defaultProgramResult) extends VM {
    override def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] =
      runFn(context.asInstanceOf[Ledger.PC]).asInstanceOf[ProgramResult[W, S]]
  }

  class MockValidatorsAlwaysSucceed extends Validators {

    override val blockValidator: BlockValidator = new BlockValidator {
      override def validateBlockAndReceipts(block: Block, receipts: Seq[Receipt]) = Right(block)
      override def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Right(Block(blockHeader, blockBody))
    }

    override val blockHeaderValidator: BlockHeaderValidator = (blockHeader: BlockHeader, blockchain: Blockchain) => Right(blockHeader)

    override val ommersValidator: OmmersValidator = (blockNumber: BigInt, ommers: Seq[BlockHeader], blockchain: Blockchain) => Right(())

    override val signedTransactionValidator: SignedTransactionValidator =
      (stx: SignedTransaction, account: Account, blockHeader: BlockHeader, upfrontGasCost: UInt256, accumGasLimit: BigInt) => Right(())
  }

  object MockValidatorsAlwaysFail extends Validators {
    override val signedTransactionValidator = new SignedTransactionValidator {
      def validate(stx: SignedTransaction, account: Account, blockHeader: BlockHeader,
                   upfrontGasCost: UInt256, accumGasLimit: BigInt) = Left(SignedTransactionError.TransactionSignatureError)
    }

    override val blockHeaderValidator = new BlockHeaderValidator {
      def validate(blockHeader: BlockHeader, blockchain: Blockchain) = Left(HeaderNumberError)
    }

    override val ommersValidator = new OmmersValidator {
      override def validate(blockNumber: BigInt, ommers: Seq[BlockHeader], blockchain: Blockchain) = Left(OmmersNotValidError)
    }

    override val blockValidator = new BlockValidator {
      def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Left(BlockTransactionsHashError)
      def validateBlockAndReceipts(block: Block, receipts: Seq[Receipt]) = Left(BlockTransactionsHashError)
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
