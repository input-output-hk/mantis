package io.iohk.ethereum.consensus

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.{BlockHeaderValidator, SignedTransactionError, SignedTransactionValid, Validators}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess}
import io.iohk.ethereum.nodebuilder.Node
import org.spongycastle.util.encoders.Hex

/**
 * Abstraction for a consensus protocol implementation.
 *
 * @see [[io.iohk.ethereum.consensus.Protocol Protocol]]
 */
// FIXME Lot's of stuff to do...
trait Consensus {
  type Config <: AnyRef /*Product*/
  // type Validators <: validators.Validators

  def protocol: Protocol

  def config: FullConsensusConfig[Config]

  /**
   * Provides the set of validators this consensus protocol demands.
   */
  def validators: Validators

  def blockGenerator: BlockGenerator

  /**
   * Returns `true` if this is the standard Ethereum PoW consensus protocol (`ethash`).
   *
   * @see [[io.iohk.ethereum.consensus.Protocol.Ethash Protocol.Ethash]]
   */
  final def isEthash: Boolean = protocol.isEthash

  /**
   * Starts the consensus protocol on the current `node`.
   */
  def startProtocol(node: Node): Unit

  /**
   * Stops the consensus protocol on the current node.
   * This is called internally when the node terminates.
   */
  def stopProtocol(): Unit

  /**
   * Provides the [[io.iohk.ethereum.consensus.validators.BlockHeaderValidator BlockHeaderValidator]] that is specific
   * to this consensus protocol.
   */
  // FIXME Probably include the whole of [[io.iohk.ethereum.consensus.validators.Validators]].
  def blockHeaderValidator: BlockHeaderValidator

  // Ledger uses this in importBlock
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

  def validateSignedTransaction(
    stx: SignedTransaction,
    senderAccount: Account,
    blockHeader: BlockHeader,
    upfrontGasCost: UInt256,
    accumGasUsed: BigInt
  ): Either[SignedTransactionError, SignedTransactionValid]
}

abstract class StdConsensus extends Consensus {
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

  def validateSignedTransaction(
    stx: SignedTransaction, senderAccount: Account,
    blockHeader: BlockHeader, upfrontGasCost: UInt256,
    accumGasUsed: BigInt
  ): Either[SignedTransactionError, SignedTransactionValid] = {

    validators.signedTransactionValidator.validate(
      stx = stx,
      senderAccount = senderAccount,
      blockHeader = blockHeader,
      upfrontGasCost = upfrontGasCost,
      accumGasUsed = accumGasUsed
    )
  }
}
