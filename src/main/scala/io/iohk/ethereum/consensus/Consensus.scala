package io.iohk.ethereum.consensus

import akka.util.ByteString
import io.iohk.ethereum.consensus.blocks.{BlockGenerator, TestBlockGenerator}
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess, BlockPreparator}
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import org.spongycastle.util.encoders.Hex

/**
 * Abstraction for a consensus protocol implementation.
 *
 * @see [[io.iohk.ethereum.consensus.Protocol Protocol]]
 */
trait Consensus {
  /**
   * The type of configuration [[io.iohk.ethereum.consensus.FullConsensusConfig#specific specific]]
   * to this consensus protocol implementation.
   */
  type Config <: AnyRef /*Product*/

  def protocol: Protocol

  def config: FullConsensusConfig[Config]

  /**
   * This is the VM used to prepare and generate blocks.
   */
  def vm: VMImpl

  /**
   * Provides the set of validators specific to this consensus protocol.
   */
  def validators: Validators

  /**
   * This is used by the [[io.iohk.ethereum.consensus.Consensus#blockGenerator blockGenerator]].
   */
  def blockPreparator: BlockPreparator

  /**
   * Returns the [[io.iohk.ethereum.consensus.blocks.BlockGenerator BlockGenerator]]
   * this consensus protocol uses.
   */
  def blockGenerator: BlockGenerator

  /**
   * Starts the consensus protocol on the current `node`.
   */
  def startProtocol(node: Node): Unit

  /**
   * Stops the consensus protocol on the current node.
   * This is called internally when the node terminates.
   */
  def stopProtocol(): Unit

  // Note Ledger uses this in importBlock
  def validateBlockBeforeExecution(
    block: Block,
    getBlockHeaderByHash: GetBlockHeaderByHash,
    getNBlocksBack: GetNBlocksBack
  ): Either[ValidationBeforeExecError, BlockExecutionSuccess]

  /**
   * This function validates that the various results from execution are consistent with the block. This includes:
   *   - Validating the resulting stateRootHash
   *   - Doing BlockValidator.validateBlockReceipts validations involving the receipts
   *   - Validating the resulting gas used
   *
   * @note This method was originally provided by the [[io.iohk.ethereum.ledger.Ledger Ledger]].
   *
   * @param block to validate
   * @param stateRootHash from the resulting state trie after executing the txs from the block
   * @param receipts associated with the execution of each of the tx from the block
   * @param gasUsed, accumulated gas used for the execution of the txs from the block
   * @return None if valid else a message with what went wrong
   */
  def validateBlockAfterExecution(
    block: Block,
    stateRootHash: ByteString,
    receipts: Seq[Receipt],
    gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess]
}

/**
 * Internal API, used for testing.
 *
 * This is a [[Consensus]] API for the needs of the test suites.
 * It gives a lot of flexibility overriding parts of a consensus' behavior
 * but it is the developer's responsibility to maintain consistency (though the
 * particular consensus protocols we implement so far do their best
 * in that direction).
 */
trait TestConsensus extends Consensus {
  def blockGenerator: TestBlockGenerator

  /** Internal API, used for testing */
  protected def newBlockGenerator(validators: Validators): TestBlockGenerator

  /** Internal API, used for testing */
  def withValidators(validators: Validators): TestConsensus

  /** Internal API, used for testing */
  def withVM(vm: VMImpl): TestConsensus

  /** Internal API, used for testing */
  def withBlockGenerator(blockGenerator: TestBlockGenerator): TestConsensus
}

abstract class ConsensusImpl[C <: AnyRef](
  theVm: VMImpl,
  blockchain: BlockchainImpl,
  blockchainConfig: BlockchainConfig,
  fullConsensusConfig: FullConsensusConfig[C]
) extends TestConsensus with Logger {

  final type Config = C

  protected val _blockPreparator = new BlockPreparator(
    vm = vm,
    signedTxValidator = validators.signedTransactionValidator,
    blockchain = blockchain,
    blockchainConfig = blockchainConfig
  )

  /**
   * This is used by the [[io.iohk.ethereum.consensus.Consensus#blockGenerator blockGenerator]].
   */
  def blockPreparator: BlockPreparator = this._blockPreparator

  /**
   * This is the VM used to prepare and generate blocks.
   */
  def vm: VMImpl = theVm

  def config: FullConsensusConfig[C] = fullConsensusConfig

  // NOTE Ethash overrides this to include ommers validation
  def validateBlockBeforeExecution(
    block: Block,
    getBlockHeaderByHash: GetBlockHeaderByHash,
    getNBlocksBack: GetNBlocksBack
  ): Either[ValidationBeforeExecError, BlockExecutionSuccess] = {
    val blockHeaderV = validators.blockHeaderValidator
    val blockV = validators.blockValidator

    val header = block.header
    val body = block.body

    val result = for {
      _ <- blockHeaderV.validate(header, getBlockHeaderByHash)
      _ <- blockV.validateHeaderAndBody(header, body)
    } yield BlockExecutionSuccess

    result.left.map(ValidationBeforeExecError)
  }

  def validateBlockAfterExecution(
    block: Block,
    stateRootHash: ByteString,
    receipts: Seq[Receipt],
    gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess] = {

    val blockV = validators.blockValidator
    val header = block.header
    val blockAndReceiptsValidation = blockV.validateBlockAndReceipts(header, receipts)

    if(header.gasUsed != gasUsed)
      Left(ValidationAfterExecError(s"Block has invalid gas used, expected ${header.gasUsed} but got $gasUsed"))
    else if(header.stateRoot != stateRootHash)
      Left(ValidationAfterExecError(
        s"Block has invalid state root hash, expected ${Hex.toHexString(header.stateRoot.toArray)} but got ${Hex.toHexString(stateRootHash.toArray)}")
      )
    else if(blockAndReceiptsValidation.isLeft)
      Left(ValidationAfterExecError(blockAndReceiptsValidation.left.get.toString))
    else
      Right(BlockExecutionSuccess)
  }
}

