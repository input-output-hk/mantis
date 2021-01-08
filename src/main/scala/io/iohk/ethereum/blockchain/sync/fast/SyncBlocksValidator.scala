package io.iohk.ethereum.blockchain.sync.fast

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.{BlockBody, BlockHeader, Blockchain}

trait SyncBlocksValidator {

  import SyncBlocksValidator._
  import BlockBodyValidationResult._

  def blockchain: Blockchain
  def validators: Validators

  def validateBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): BlockBodyValidationResult = {
    val headersWithBodies = zipHeadersWithBodies(requestedHashes, blockBodies)
    findInvalidHeaderWithBody(headersWithBodies).getOrElse(Valid)
  }

  private def zipHeadersWithBodies(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]) = {
    requestedHashes.zip(blockBodies).map { case (hash, body) => (blockchain.getBlockHeaderByHash(hash), body) }
  }

  private def findInvalidHeaderWithBody(headersWithBodies: Seq[(Option[BlockHeader], BlockBody)]) = {
    headersWithBodies.collectFirst {
      case (Some(header), body) if !checkHeaderWithBody(header, body) => Invalid
      case (None, _) => DbError
    }
  }

  private def checkHeaderWithBody(header: BlockHeader, body: BlockBody) = {
    validators.blockValidator.validateHeaderAndBody(header, body).isRight
  }

  def checkHeadersChain(headers: Seq[BlockHeader], requestedNum: BigInt, syncState: SyncingHandlerState): Boolean = {
    checkFulfillsRequest(headers, requestedNum, syncState) && checkHeadersAreChained(headers)
  }

  private def checkFulfillsRequest(blockHeaders: Seq[BlockHeader], requestedNum: BigInt, syncState: SyncingHandlerState) = {
    blockHeaders.nonEmpty &&
    blockHeaders.size <= requestedNum &&
    blockHeaders.head.number == syncState.bestBlockHeaderNumber + 1
  }

  private def checkHeadersAreChained(headers: Seq[BlockHeader]) = {
    headers.length <= 1 || headers.zip(headers.tail).forall(checkParentWithChild)
  }

  private def checkParentWithChild(pair: (BlockHeader, BlockHeader)) = {
    val (parent, child) = pair
    parent.hash == child.parentHash && parent.number + 1 == child.number
  }
}

object SyncBlocksValidator {
  sealed trait BlockBodyValidationResult
  object BlockBodyValidationResult {
    case object Valid extends BlockBodyValidationResult
    case object Invalid extends BlockBodyValidationResult
    case object DbError extends BlockBodyValidationResult
  }
}
