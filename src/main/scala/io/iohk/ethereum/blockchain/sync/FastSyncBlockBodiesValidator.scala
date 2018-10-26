package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.BlockValid
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

trait FastSyncBlockBodiesValidator {

  import FastSyncBlockBodiesValidator._
  import BlockBodyValidationResult._

  def blockchain: Blockchain
  def validators: Validators

  /** Validates whether the received block bodies match the block headers stored on the blockchain,
    * returning the valid block bodies
    *
    * @param requestedHashes hash of the blocks to which the requested bodies should belong
    * @param blockBodies     received from peer
    *
    * @return the block validation result
    */
  def validateBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): BlockBodyValidationResult = {
    var result: BlockBodyValidationResult = Valid
    (requestedHashes zip blockBodies)
      .map { case (hash, body) => (blockchain.getBlockHeaderByHash(hash), body) }
      .forall {
        case (Some(header), body) =>
          val validationResult: Either[StdBlockValidator.BlockError, BlockValid] = validators.blockValidator.validateHeaderAndBody(header, body)
          result = validationResult.fold(_ => Invalid, _ => Valid)
          validationResult.isRight
        case _ =>
          result = DbError
          false
      }
    result
  }
}

object FastSyncBlockBodiesValidator {
  sealed trait BlockBodyValidationResult
  object BlockBodyValidationResult {
    case object Valid   extends BlockBodyValidationResult
    case object Invalid extends BlockBodyValidationResult
    case object DbError extends BlockBodyValidationResult
  }
}
