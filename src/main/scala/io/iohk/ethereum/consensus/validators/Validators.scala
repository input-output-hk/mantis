package io.iohk.ethereum.consensus.validators

import akka.util.ByteString

import io.iohk.ethereum.consensus.GetBlockHeaderByHash
import io.iohk.ethereum.consensus.GetNBlocksBack
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.ledger.BlockExecutionError
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.BlockExecutionSuccess
import io.iohk.ethereum.utils.BlockchainConfig

trait Validators {
  def blockValidator: BlockValidator
  def blockHeaderValidator: BlockHeaderValidator
  def signedTransactionValidator: SignedTransactionValidator

  // Note BlockImport uses this in importBlock
  def validateBlockBeforeExecution(
      block: Block,
      getBlockHeaderByHash: GetBlockHeaderByHash,
      getNBlocksBack: GetNBlocksBack
  )(implicit blockchainConfig: BlockchainConfig): Either[ValidationBeforeExecError, BlockExecutionSuccess]

  /** This function validates that the various results from execution are consistent with the block. This includes:
    *   - Validating the resulting stateRootHash
    *   - Doing BlockValidator.validateBlockReceipts validations involving the receipts
    *   - Validating the resulting gas used
    *
    * @param block         to validate
    * @param stateRootHash from the resulting state trie after executing the txs from the block
    * @param receipts      associated with the execution of each of the tx from the block
    * @param gasUsed       accumulated gas used for the execution of the txs from the block
    * @return None if valid else a message with what went wrong
    */
  def validateBlockAfterExecution(
      block: Block,
      stateRootHash: ByteString,
      receipts: Seq[Receipt],
      gasUsed: BigInt
  )(implicit blockchainConfig: BlockchainConfig): Either[BlockExecutionError, BlockExecutionSuccess]
}
