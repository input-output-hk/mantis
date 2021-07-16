package io.iohk.ethereum.testmode

import akka.util.ByteString

import monix.eval.Task

import io.iohk.ethereum.consensus.blocks.BlockTimestampProvider
import io.iohk.ethereum.consensus.blocks.NoOmmersBlockGenerator
import io.iohk.ethereum.consensus.blocks.TestBlockGenerator
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.mining.FullMiningConfig
import io.iohk.ethereum.consensus.mining.GetBlockHeaderByHash
import io.iohk.ethereum.consensus.mining.GetNBlocksBack
import io.iohk.ethereum.consensus.mining.Mining
import io.iohk.ethereum.consensus.mining.MiningConfig
import io.iohk.ethereum.consensus.mining.Protocol
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerProtocol
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponse
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses.MinerNotExist
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator
import io.iohk.ethereum.consensus.validators.std.StdSignedTransactionValidator
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.ledger.BlockExecutionError
import io.iohk.ethereum.ledger.BlockExecutionSuccess
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.utils.BlockchainConfig

class TestmodeMining(
    override val vm: VMImpl,
    evmCodeStorage: EvmCodeStorage,
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    miningConfig: MiningConfig,
    node: TestNode,
    blockTimestamp: Long = 0
) // var, because it can be modified by test_ RPC endpoints
    extends Mining {

  override type Config = AnyRef
  override def protocol: Protocol = Protocol.PoW
  override def config: FullMiningConfig[AnyRef] = FullMiningConfig[AnyRef](miningConfig, "")

  override def difficultyCalculator: DifficultyCalculator = DifficultyCalculator

  class TestValidators extends Validators {
    override def blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidator {
      override def validate(
          blockHeader: BlockHeader,
          getBlockHeaderByHash: GetBlockHeaderByHash
      )(implicit blockchainConfig: BlockchainConfig): Either[BlockHeaderError, BlockHeaderValid] = Right(
        BlockHeaderValid
      )

      override def validateHeaderOnly(blockHeader: BlockHeader)(implicit
          blockchainConfig: BlockchainConfig
      ): Either[BlockHeaderError, BlockHeaderValid] =
        Right(BlockHeaderValid)
    }
    override def signedTransactionValidator: SignedTransactionValidator = StdSignedTransactionValidator
    override def validateBlockBeforeExecution(
        block: Block,
        getBlockHeaderByHash: GetBlockHeaderByHash,
        getNBlocksBack: GetNBlocksBack
    )(implicit
        blockchainConfig: BlockchainConfig
    ): Either[BlockExecutionError.ValidationBeforeExecError, BlockExecutionSuccess] = Right(BlockExecutionSuccess)
    override def validateBlockAfterExecution(
        block: Block,
        stateRootHash: ByteString,
        receipts: Seq[Receipt],
        gasUsed: BigInt
    )(implicit blockchainConfig: BlockchainConfig): Either[BlockExecutionError, BlockExecutionSuccess] = Right(
      BlockExecutionSuccess
    )
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

  override def validators: Validators = ValidatorsExecutor.apply(Protocol.MockedPow)

  override def blockPreparator: BlockPreparator = new BlockPreparator(
    vm = vm,
    signedTxValidator = validators.signedTransactionValidator,
    blockchain = blockchain,
    blockchainReader = blockchainReader
  ) {
    override def payBlockReward(block: Block, worldStateProxy: InMemoryWorldStateProxy)(implicit
        blockchainConfig: BlockchainConfig
    ): InMemoryWorldStateProxy =
      node.sealEngine match {
        case SealEngineType.NoProof =>
          super.payBlockReward(block, worldStateProxy)
        case SealEngineType.NoReward =>
          worldStateProxy
      }
  }

  override def blockGenerator: NoOmmersBlockGenerator =
    new NoOmmersBlockGenerator(
      evmCodeStorage,
      miningConfig,
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

  /** Sends msg to the internal miner and waits for the response
    */
  override def askMiner(msg: MockedMinerProtocol): Task[MockedMinerResponse] = Task.now(MinerNotExist)

  /** Sends msg to the internal miner
    */
  override def sendMiner(msg: MinerProtocol): Unit = {}
}
