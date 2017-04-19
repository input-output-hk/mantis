package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.domain.{Block, BlockchainStorages}
import io.iohk.ethereum.ledger.BlockExecutionError.TxsExecutionError
import io.iohk.ethereum.ledger.{BlockExecutionError, Ledger}
import io.iohk.ethereum.validators.Validators
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

}
