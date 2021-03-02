package io.iohk.ethereum.blockchain.sync.fast

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, Validators}
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.BlockValid
import io.iohk.ethereum.domain.{BlockBody, BlockHeader, Blockchain}

trait SyncBlocksValidator {

  import SyncBlocksValidator._
  import BlockBodyValidationResult._

  def blockchain: Blockchain
  def validators: Validators

  def validateBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): BlockBodyValidationResult = {
    var result: BlockBodyValidationResult = Valid
    (requestedHashes zip blockBodies)
      .map { case (hash, body) => (blockchain.getBlockHeaderByHash(hash), body) }
      .forall {
        case (Some(header), body) =>
          val validationResult: Either[StdBlockValidator.BlockError, BlockValid] =
            validators.blockValidator.validateHeaderAndBody(header, body)
          result = validationResult.fold(_ => Invalid, _ => Valid)
          validationResult.isRight
        case _ =>
          result = DbError
          false
      }
    result
  }

  def validateHeaderOnly(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    validators.blockHeaderValidator.validateHeaderOnly(blockHeader)

  def checkHeadersChain(headers: Seq[BlockHeader]): Boolean =
    if (headers.length > 1) headers.zip(headers.tail).forall { case (parent, child) =>
      parent.hash == child.parentHash && parent.number + 1 == child.number
    }
    else true
}

object SyncBlocksValidator {
  sealed trait BlockBodyValidationResult
  object BlockBodyValidationResult {
    case object Valid extends BlockBodyValidationResult
    case object Invalid extends BlockBodyValidationResult
    case object DbError extends BlockBodyValidationResult
  }
}
