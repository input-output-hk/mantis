package io.iohk.ethereum.blockchain.sync.fast

import akka.event.LoggingAdapter
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.{ BlockHeader, Blockchain }
import io.iohk.ethereum.network.Peer

trait FastSyncBlockHeadersValidator {

  import FastSyncBlockHeadersValidator._

  def blockchain: Blockchain
  def validators: Validators
  def log: LoggingAdapter

  /** Validates received header
    *
    * @param header                   header to validate
    * @param peer                     peer which send the header
    * @param nextBlockToFullyValidate block number of the next block to validate fully
    * @return the valid blockHeader with flag which tells if validationState should be updated
    *         or the error encountered during validation
    */
  def validateHeader(
    header: BlockHeader,
    peer: Peer,
    nextBlockToFullyValidate: BigInt
  ): Either[HeaderProcessingResult, (BlockHeader, Boolean)] = {
    if (header.number >= nextBlockToFullyValidate) {
      validators.blockHeaderValidator.validate(header, blockchain.getBlockHeaderByHash) match {
        case Right(_) =>
          Right((header, true))

        case Left(error) =>
          log.warning("Block header validation failed during fast sync at block {}: {}", header.number, error)
          Left(ValidationFailed(header, peer))
      }
    } else {
      Right((header, false))
    }
  }

  def areHeadersFormingChain(headers: Seq[BlockHeader]): Boolean = {
    if (headers.length > 1) headers.zip(headers.tail).forall{case (parent, child) =>
      parent.hash == child.parentHash && parent.number + 1 == child.number
    } else true
  }
}

object FastSyncBlockHeadersValidator {

  sealed trait HeaderProcessingResult

  case object HeadersProcessingFinished extends HeaderProcessingResult
  case object ImportedTargetBlock extends HeaderProcessingResult
  case class ParentDifficultyNotFound(header: BlockHeader) extends HeaderProcessingResult
  case class ValidationFailed(header: BlockHeader, peer: Peer) extends HeaderProcessingResult

}
