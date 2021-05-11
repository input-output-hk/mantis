package io.iohk.ethereum.testmode

import akka.util.ByteString
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.consensus.blocks.{BlockTimestampProvider, NoOmmersBlockGenerator, TestBlockGenerator}
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.{MockedMinerProtocol, MockedMinerResponse}
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses.MinerNotExist
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.consensus.validators.std.{StdBlockValidator, StdSignedTransactionValidator}
import io.iohk.ethereum.domain.{Block, BlockBody, BlockHeader, BlockchainImpl, Receipt}
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess, BlockPreparator}
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.utils.BlockchainConfig
import monix.eval.Task
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor

class TestmodeConsensus(
    override val vm: VMImpl,
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    consensusConfig: ConsensusConfig,
    override val difficultyCalculator: DifficultyCalculator,
    var blockTimestamp: Long = 0
) // var, because it can be modified by test_ RPC endpoints
    extends Consensus {

  override type Config = AnyRef
  override def protocol: Protocol = Protocol.PoW
  override def config: FullConsensusConfig[AnyRef] = FullConsensusConfig[AnyRef](consensusConfig, "")

  class TestValidators extends Validators {
    override def blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidator {
      override def validate(
          blockHeader: BlockHeader,
          getBlockHeaderByHash: GetBlockHeaderByHash
      ): Either[BlockHeaderError, BlockHeaderValid] = Right(BlockHeaderValid)

      override def validateHeaderOnly(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
        Right(BlockHeaderValid)
    }
    override def signedTransactionValidator: SignedTransactionValidator =
      new StdSignedTransactionValidator(blockchainConfig)
    override def validateBlockBeforeExecution(
        block: Block,
        getBlockHeaderByHash: GetBlockHeaderByHash,
        getNBlocksBack: GetNBlocksBack
    ): Either[BlockExecutionError.ValidationBeforeExecError, BlockExecutionSuccess] = Right(BlockExecutionSuccess)
    override def validateBlockAfterExecution(
        block: Block,
        stateRootHash: ByteString,
        receipts: Seq[Receipt],
        gasUsed: BigInt
    ): Either[BlockExecutionError, BlockExecutionSuccess] = Right(BlockExecutionSuccess)
    override def blockValidator: BlockValidator = new BlockValidator {
      override def validateBlockAndReceipts(
          blockHeader: BlockHeader,
          receipts: Seq[Receipt]
      ): Either[StdBlockValidator.BlockError, StdBlockValidator.BlockValid] = Right(StdBlockValidator.BlockValid)
      override def validateHeaderAndBody(
          blockHeader: BlockHeader,
          blockBody: BlockBody
      ): Either[StdBlockValidator.BlockError, StdBlockValidator.BlockValid] = Right(StdBlockValidator.BlockValid)
    }
  }

  override def validators: Validators = ValidatorsExecutor.apply(blockchainConfig, Protocol.MockedPow)

  override val blockPreparator: BlockPreparator = new BlockPreparator(
    vm = vm,
    signedTxValidator = validators.signedTransactionValidator,
    blockchain = blockchain,
    blockchainConfig = blockchainConfig
  )

  override val blockGenerator =
    new NoOmmersBlockGenerator(
      blockchain,
      blockchainConfig,
      consensusConfig,
      blockPreparator,
      difficultyCalculator,
      new BlockTimestampProvider {
        override def getEpochSecond: Long = blockTimestamp
      }
    ) {
      override def withBlockTimestampProvider(blockTimestampProvider: BlockTimestampProvider): TestBlockGenerator = this

    }

  override def startProtocol(node: Node): Unit = {}
  override def stopProtocol(): Unit = {}

  /**
    * Sends msg to the internal miner and waits for the response
    */
  override def askMiner(msg: MockedMinerProtocol): Task[MockedMinerResponse] = Task.now(MinerNotExist)

  /**
    * Sends msg to the internal miner
    */
  override def sendMiner(msg: MinerProtocol): Unit = {}
}

trait TestmodeConsensusBuilder extends ConsensusBuilder {
  self: VmBuilder with BlockchainBuilder with BlockchainConfigBuilder with ConsensusConfigBuilder =>

  override lazy val consensus = new TestmodeConsensus(
    vm,
    blockchain,
    blockchainConfig,
    consensusConfig,
    DifficultyCalculator(blockchainConfig)
  )
}
