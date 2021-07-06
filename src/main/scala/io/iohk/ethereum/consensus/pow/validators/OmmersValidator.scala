package io.iohk.ethereum.consensus.pow.validators

import akka.util.ByteString

import io.iohk.ethereum.consensus.mining.GetBlockHeaderByHash
import io.iohk.ethereum.consensus.mining.GetNBlocksBack
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator.OmmersError
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator.OmmersValid
import io.iohk.ethereum.consensus.validators.BlockHeaderError
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainReader

trait OmmersValidator {

  def validate(
      parentHash: ByteString,
      blockNumber: BigInt,
      ommers: Seq[BlockHeader],
      getBlockByHash: GetBlockHeaderByHash,
      getNBlocksBack: GetNBlocksBack
  ): Either[OmmersError, OmmersValid]

  def validate(
      parentHash: ByteString,
      blockNumber: BigInt,
      ommers: Seq[BlockHeader],
      blockchainReader: BlockchainReader
  ): Either[OmmersError, OmmersValid] = {

    val getBlockHeaderByHash: ByteString => Option[BlockHeader] = blockchainReader.getBlockHeaderByHash
    val getNBlocksBack: (ByteString, Int) => List[Block] =
      (_, n) => ((blockNumber - n) until blockNumber).toList.flatMap(blockchainReader.getBestBranch.getBlockByNumber)

    validate(parentHash, blockNumber, ommers, getBlockHeaderByHash, getNBlocksBack)
  }

}

object OmmersValidator {
  sealed trait OmmersError

  object OmmersError {
    case object OmmersLengthError extends OmmersError
    case class OmmersHeaderError(errors: List[BlockHeaderError]) extends OmmersError
    case object OmmersUsedBeforeError extends OmmersError
    case object OmmerIsAncestorError extends OmmersError
    case object OmmerParentIsNotAncestorError extends OmmersError
    case object OmmersDuplicatedError extends OmmersError
  }

  sealed trait OmmersValid
  case object OmmersValid extends OmmersValid
}
