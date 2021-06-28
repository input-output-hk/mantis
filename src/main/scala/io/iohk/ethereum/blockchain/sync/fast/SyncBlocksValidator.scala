package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.ActorLogging
import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, Validators}
import io.iohk.ethereum.domain.{BlockBody, BlockHeader, BlockchainReader}

trait SyncBlocksValidator { this: ActorLogging =>

  import SyncBlocksValidator._
  import BlockBodyValidationResult._

  def blockchainReader: BlockchainReader
  def validators: Validators

  def validateBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): BlockBodyValidationResult =
    (requestedHashes zip blockBodies)
      .map { case (hash, body) => (blockchainReader.getBlockHeaderByHash(hash), body) }
      .foldLeft[BlockBodyValidationResult](Valid) {
        case (Valid, (Some(header), body)) =>
          validators.blockValidator
            .validateHeaderAndBody(header, body)
            .fold(
              { error =>
                log.error(s"Block body validation failed with error $error")
                Invalid
              },
              _ => Valid
            )
        case (Valid, _) => DbError
        case (invalid, _) => invalid
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
