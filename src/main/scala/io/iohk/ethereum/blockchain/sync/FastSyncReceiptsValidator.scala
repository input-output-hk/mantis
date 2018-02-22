package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.{Blockchain, Receipt}
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.BlockError

trait FastSyncReceiptsValidator {

  import FastSyncReceiptsValidator._
  import ReceiptsValidationResult._

  def blockchain: Blockchain
  def validators: Validators

  /**
    * Validates whether the received receipts match the block headers stored on the blockchain,
    * returning the valid receipts
    *
    * @param requestedHashes hash of the blocks to which the requested receipts should belong
    * @param receipts received by the peer
    * @return the valid receipts or the error encountered while validating them
    */
  def validateReceipts(requestedHashes: Seq[ByteString], receipts: Seq[Seq[Receipt]]): ReceiptsValidationResult = {
    val blockHashesWithReceipts = requestedHashes.zip(receipts)
    val blockHeadersWithReceipts = blockHashesWithReceipts.map{ case (hash, blockReceipts) =>
      blockchain.getBlockHeaderByHash(hash) -> blockReceipts }

    val receiptsValidationError = blockHeadersWithReceipts.collectFirst {
      case (Some(header), receipt) if validators.blockValidator.validateBlockAndReceipts(header, receipt).isLeft =>
        Invalid(validators.blockValidator.validateBlockAndReceipts(header, receipt).left.get)
      case (None, _) => DbError
    }
    receiptsValidationError.getOrElse(Valid(blockHashesWithReceipts))
  }

}

object FastSyncReceiptsValidator {
  sealed trait ReceiptsValidationResult
  object ReceiptsValidationResult {
    case class Valid(blockHashesAndReceipts: Seq[(ByteString, Seq[Receipt])]) extends ReceiptsValidationResult
    case class Invalid(error: BlockError) extends ReceiptsValidationResult
    case object DbError extends ReceiptsValidationResult
  }
}
