package io.iohk.ethereum.consensus.pow.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator.{OmmersError, OmmersValid}
import io.iohk.ethereum.consensus.validators.BlockHeaderError
import io.iohk.ethereum.consensus.{GetBlockHeaderByHash, GetNBlocksBack}
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain, BlockchainReader}

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
      blockchain: Blockchain,
      blockchainReader: BlockchainReader
  ): Either[OmmersError, OmmersValid] = {

    val getBlockHeaderByHash: ByteString => Option[BlockHeader] = blockchainReader.getBlockHeaderByHash
    val getNBlocksBack: (ByteString, Int) => List[Block] =
      (_, n) => ((blockNumber - n) until blockNumber).toList.flatMap(blockchainReader.getBlockByNumber)

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
