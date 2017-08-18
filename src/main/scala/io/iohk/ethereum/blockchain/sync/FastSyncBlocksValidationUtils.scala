package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.validators.{BlockValidator, Validators}

trait FastSyncBlocksValidationUtils {

  import FastSyncBlocksValidationUtils._
  import BlockBodyValidationResult._

  def blockchain: Blockchain
  def validators: Validators

  def validateBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): BlockBodyValidationResult = {
    var result: BlockBodyValidationResult = Valid
    (requestedHashes zip blockBodies)
      .map { case (hash, body) => (blockchain.getBlockHeaderByHash(hash), body) }
      .forall {
        case (Some(header), body) =>
          val validationResult: Either[BlockValidator.BlockError, Block] = validators.blockValidator.validateHeaderAndBody(header, body)
          result = validationResult.fold(_ => Invalid, _ => Valid)
          validationResult.isRight
        case _ =>
          result = DbError
          false
      }
    result
  }

}

object FastSyncBlocksValidationUtils {
  sealed trait BlockBodyValidationResult
  object BlockBodyValidationResult {
    case object Valid extends BlockBodyValidationResult
    case object Invalid extends BlockBodyValidationResult
    case object DbError extends BlockBodyValidationResult
  }
}
