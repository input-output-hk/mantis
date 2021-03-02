package io.iohk.ethereum

import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator.OmmersError.OmmersNotValidError
import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator.OmmersValid
import io.iohk.ethereum.consensus.ethash.validators.{OmmersValidator, ValidatorsExecutor}
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderNumberError
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.{BlockError, BlockTransactionsHashError, BlockValid}
import io.iohk.ethereum.consensus.{Consensus, GetBlockHeaderByHash, GetNBlocksBack}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationAfterExecError
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, RemoteStatus}
import io.iohk.ethereum.network.handshaker.{ConnectedState, DisconnectedState, Handshaker, HandshakerState}
import io.iohk.ethereum.vm._
import monix.eval.Task
import monix.execution.Scheduler

object Mocks {

  class MockLedger(blockchain: BlockchainImpl, shouldExecuteCorrectly: (Block, BlockchainImpl) => Boolean)
      extends Ledger {
    def consensus: Consensus = ??? // FIXME Implement

    override def checkBlockStatus(blockHash: ByteString): BlockStatus = ??? // FIXME Implement

    override def getBlockByHash(hash: ByteString): Option[Block] = ???

    override def importBlock(block: Block)(implicit blockExecutionContext: Scheduler): Task[BlockImportResult] = ???

    override def resolveBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult = ???
  }

  private val defaultProgramResult: Ledger.PC => Ledger.PR = context =>
    ProgramResult(
      returnData = ByteString.empty,
      gasRemaining = 1000000 - 25000,
      world = context.world,
      addressesToDelete = Set.empty,
      logs = Nil,
      internalTxs = Nil,
      gasRefund = 20000,
      error = None
    )

  class MockVM(runFn: Ledger.PC => Ledger.PR = defaultProgramResult) extends Ledger.VMImpl {
    override def run(context: Ledger.PC): Ledger.PR =
      runFn(context)
  }

  class MockValidatorsFailingOnBlockBodies extends MockValidatorsAlwaysSucceed {

    override val blockValidator: BlockValidator = new BlockValidator {
      override def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]) = Right(BlockValid)
      override def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Left(
        BlockTransactionsHashError
      )
    }
  }

  class MockValidatorsAlwaysSucceed extends ValidatorsExecutor {

    override val blockValidator: BlockValidator = new BlockValidator {
      override def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]) = Right(BlockValid)
      override def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Right(BlockValid)
    }

    override val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidator {
      override def validate(
          blockHeader: BlockHeader,
          getBlockHeaderByHash: GetBlockHeaderByHash
      ): Either[BlockHeaderError, BlockHeaderValid] = Right(BlockHeaderValid)

      override def validateHeaderOnly(
          blockHeader: BlockHeader
      ): Either[BlockHeaderError, BlockHeaderValid] = Right(BlockHeaderValid)
    }

    override val ommersValidator: OmmersValidator =
      (_: ByteString, _: BigInt, _: Seq[BlockHeader], _: GetBlockHeaderByHash, _: GetNBlocksBack) => Right(OmmersValid)

    override val signedTransactionValidator: SignedTransactionValidator =
      (_: SignedTransaction, _: Account, _: BlockHeader, _: UInt256, _: BigInt) => Right(SignedTransactionValid)
  }

  object MockValidatorsAlwaysSucceed extends MockValidatorsAlwaysSucceed

  object MockValidatorsAlwaysFail extends ValidatorsExecutor {
    override val signedTransactionValidator: SignedTransactionValidator =
      (_: SignedTransaction, _: Account, _: BlockHeader, _: UInt256, _: BigInt) =>
        Left(SignedTransactionError.TransactionSignatureError)

    override val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidator {
      override def validate(
          blockHeader: BlockHeader,
          getBlockHeaderByHash: GetBlockHeaderByHash
      ): Either[BlockHeaderError, BlockHeaderValid] = Left(HeaderNumberError)

      override def validateHeaderOnly(blockHeader: BlockHeader) = Left(HeaderNumberError)
    }

    override val ommersValidator: OmmersValidator =
      (_: ByteString, _: BigInt, _: Seq[BlockHeader], _: GetBlockHeaderByHash, _: GetNBlocksBack) =>
        Left(OmmersNotValidError)

    override val blockValidator: BlockValidator = new BlockValidator {
      override def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody) = Left(
        BlockTransactionsHashError
      )
      override def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]) = Left(
        BlockTransactionsHashError
      )
    }
  }

  class MockValidatorsFailOnSpecificBlockNumber(number: BigInt) extends MockValidatorsAlwaysSucceed {
    override val blockValidator: BlockValidator = new BlockValidator {
      override def validateHeaderAndBody(
          blockHeader: BlockHeader,
          blockBody: BlockBody
      ): Either[BlockError, BlockValid] = {
        if (blockHeader.number == number) Left(BlockTransactionsHashError) else Right(BlockValid)
      }
      override def validateBlockAndReceipts(
          blockHeader: BlockHeader,
          receipts: Seq[Receipt]
      ): Either[BlockError, BlockValid] = {
        if (blockHeader.number == number) Left(BlockTransactionsHashError) else Right(BlockValid)
      }
    }

    override def validateBlockAfterExecution(
        block: Block,
        stateRootHash: ByteString,
        receipts: Seq[Receipt],
        gasUsed: BigInt
    ): Either[BlockExecutionError, BlockExecutionSuccess] = {
      if (block.header.number == number) Left(ValidationAfterExecError("")) else Right(BlockExecutionSuccess)
    }
  }

  case class MockHandshakerAlwaysSucceeds(
      initialStatus: RemoteStatus,
      currentMaxBlockNumber: BigInt,
      forkAccepted: Boolean
  ) extends Handshaker[PeerInfo] {
    override val handshakerState: HandshakerState[PeerInfo] =
      ConnectedState(
        PeerInfo(
          initialStatus,
          initialStatus.chainWeight,
          forkAccepted,
          currentMaxBlockNumber,
          initialStatus.bestHash
        )
      )
    override def copy(handshakerState: HandshakerState[PeerInfo]): Handshaker[PeerInfo] = this
  }

  case class MockHandshakerAlwaysFails(reason: Int) extends Handshaker[PeerInfo] {
    override val handshakerState: HandshakerState[PeerInfo] = DisconnectedState(reason)

    override def copy(handshakerState: HandshakerState[PeerInfo]): Handshaker[PeerInfo] = this
  }

}
