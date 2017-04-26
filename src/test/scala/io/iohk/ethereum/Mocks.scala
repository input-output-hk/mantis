package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.TxsExecutionError
import io.iohk.ethereum.ledger.{BlockExecutionError, Ledger}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.validators.BlockHeaderError.HeaderNumberError
import io.iohk.ethereum.validators.BlockValidator.BlockTransactionsHashError
import io.iohk.ethereum.validators.OmmersValidator.OmmersError.OmmersNotValidError
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm._

object Mocks {

  class MockLedger(shouldExecuteCorrectly: (Block, BlockchainStorages, Validators) => Boolean) extends Ledger{
    override def executeBlock(block: Block, storages: BlockchainStorages, validators: Validators)
    : Either[BlockExecutionError, Unit] = {
      if(shouldExecuteCorrectly(block, storages, validators))
        Right(())
      else
        Left(TxsExecutionError("StubLedger was set to fail for this case"))
    }
  }

  private val defaultProgramResult: Ledger.PC => Ledger.PR = context => ProgramResult(
    returnData = ByteString.empty,
    gasRemaining = 1000000 - 25000,
    world = context.world,
    addressesToDelete = Nil,
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

    override val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidator {
      override def validate(blockHeader: BlockHeader, blockchain: Blockchain) = Right(blockHeader)
    }

    override val ommersValidator: OmmersValidator = new OmmersValidator {
      override def validate(blockNumber: BigInt, ommers: Seq[BlockHeader], blockchain: Blockchain) = Right(())
    }

    override val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidator {
      override def validate(stx: SignedTransaction, account: Account, blockHeader: BlockHeader,
                            upfrontGasCost: UInt256, accumGasLimit: BigInt) = Right(())
    }
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
}
