package io.iohk.ethereum

import akka.util.ByteString

import io.iohk.ethereum.consensus.GetBlockHeaderByHash
import io.iohk.ethereum.consensus.GetNBlocksBack
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator.OmmersError.OmmersHeaderError
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator.OmmersValid
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderDifficultyError
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderNumberError
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.BlockError
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.BlockTransactionsHashError
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.BlockValid
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationAfterExecError
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.handshaker.ConnectedState
import io.iohk.ethereum.network.handshaker.DisconnectedState
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.handshaker.HandshakerState
import io.iohk.ethereum.vm._
import io.iohk.ethereum.utils.BlockchainConfig

object Mocks {
  private val defaultProgramResult: PC => PR = context =>
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

  class MockVM(runFn: PC => PR = defaultProgramResult) extends VMImpl {
    override def run(context: PC): PR =
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
      )(implicit blockchainConfig: BlockchainConfig): Either[BlockHeaderError, BlockHeaderValid] = Right(
        BlockHeaderValid
      )

      override def validateHeaderOnly(
          blockHeader: BlockHeader
      )(implicit blockchainConfig: BlockchainConfig): Either[BlockHeaderError, BlockHeaderValid] = Right(
        BlockHeaderValid
      )
    }

    override val ommersValidator: OmmersValidator = new OmmersValidator {
      def validate(
          parentHash: ByteString,
          blockNumber: BigInt,
          ommers: Seq[BlockHeader],
          getBlockByHash: GetBlockHeaderByHash,
          getNBlocksBack: GetNBlocksBack
      )(implicit blockchainConfig: BlockchainConfig): Either[OmmersValidator.OmmersError, OmmersValid] = Right(
        OmmersValid
      )
    }

    override val signedTransactionValidator: SignedTransactionValidator =
      new SignedTransactionValidator {
        def validate(
            stx: SignedTransaction,
            senderAccount: Account,
            blockHeader: BlockHeader,
            upfrontGasCost: UInt256,
            accumGasUsed: BigInt
        )(implicit blockchainConfig: BlockchainConfig): Either[SignedTransactionError, SignedTransactionValid] =
          Right(SignedTransactionValid)
      }
  }

  object MockValidatorsAlwaysSucceed extends MockValidatorsAlwaysSucceed

  object MockValidatorsAlwaysFail extends ValidatorsExecutor {
    override val signedTransactionValidator: SignedTransactionValidator =
      new SignedTransactionValidator {
        def validate(
            stx: SignedTransaction,
            senderAccount: Account,
            blockHeader: BlockHeader,
            upfrontGasCost: UInt256,
            accumGasUsed: BigInt
        )(implicit blockchainConfig: BlockchainConfig): Either[SignedTransactionError, SignedTransactionValid] =
          Left(SignedTransactionError.TransactionSignatureError)
      }

    override val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidator {
      override def validate(
          blockHeader: BlockHeader,
          getBlockHeaderByHash: GetBlockHeaderByHash
      )(implicit blockchainConfig: BlockchainConfig): Either[BlockHeaderError, BlockHeaderValid] = Left(
        HeaderNumberError
      )

      override def validateHeaderOnly(blockHeader: BlockHeader)(implicit blockchainConfig: BlockchainConfig) = Left(
        HeaderNumberError
      )
    }

    override val ommersValidator: OmmersValidator = new OmmersValidator {
      def validate(
          parentHash: ByteString,
          blockNumber: BigInt,
          ommers: Seq[BlockHeader],
          getBlockByHash: GetBlockHeaderByHash,
          getNBlocksBack: GetNBlocksBack
      )(implicit blockchainConfig: BlockchainConfig): Either[OmmersValidator.OmmersError, OmmersValid] =
        Left(OmmersHeaderError(List(HeaderDifficultyError)))
    }

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
      ): Either[BlockError, BlockValid] =
        if (blockHeader.number == number) Left(BlockTransactionsHashError) else Right(BlockValid)
      override def validateBlockAndReceipts(
          blockHeader: BlockHeader,
          receipts: Seq[Receipt]
      ): Either[BlockError, BlockValid] =
        if (blockHeader.number == number) Left(BlockTransactionsHashError) else Right(BlockValid)
    }

    override def validateBlockAfterExecution(
        block: Block,
        stateRootHash: ByteString,
        receipts: Seq[Receipt],
        gasUsed: BigInt
    )(implicit blockchainConfig: BlockchainConfig): Either[BlockExecutionError, BlockExecutionSuccess] =
      if (block.header.number == number) Left(ValidationAfterExecError("")) else Right(BlockExecutionSuccess)
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
