package io.iohk.ethereum.consensus.validators

import io.iohk.ethereum.consensus.validators.BlockHeaderError._
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.ledger.BloomFilter
import io.iohk.ethereum.utils.{BlockchainConfig, ByteStringUtils}

/** Validator specialized for the block with checkpoint
  *
  * @param blockchainConfig
  */
class BlockWithCheckpointHeaderValidator(blockchainConfig: BlockchainConfig) {

  def validate(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    for {
      _ <- validateCheckpointSignatures(blockHeader, parentHeader)
      _ <- validateEmptyFields(blockHeader)
      _ <- validateFieldsCopiedFromParent(blockHeader, parentHeader)
      _ <- validateGasUsed(blockHeader)
      _ <- validateTimestamp(blockHeader, parentHeader)
      _ <- validateTreasuryOptOut(blockHeader)
    } yield BlockHeaderValid
  }

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.checkpoint]] signatures
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderInvalidCheckpointSignatures]] otherwise
    */
  private def validateCheckpointSignatures(
      blockHeader: BlockHeader,
      parentHeader: BlockHeader
  ): Either[BlockHeaderError, BlockHeaderValid] = {
    blockHeader.checkpoint
      .map { checkpoint =>
        lazy val signaturesWithRecoveredKeys = checkpoint.signatures.map(s => s -> s.publicKey(parentHeader.hash))

        // if at least 2 different signatures came from the same signer it will be in this set (also takes care
        // of duplicate signatures)
        lazy val repeatedSigners = signaturesWithRecoveredKeys
          .groupBy(_._2)
          .filter(_._2.size > 1)
          .keySet
          .flatten

        lazy val (validSignatures, invalidSignatures) = signaturesWithRecoveredKeys.partition {
          //signatures are valid if the signers are known AND distinct
          case (sig, Some(pk)) => blockchainConfig.checkpointPubKeys.contains(pk) && !repeatedSigners.contains(pk)
          case _ => false
        }

        // we fail fast if there are too many signatures (DoS protection)
        if (checkpoint.signatures.size > blockchainConfig.checkpointPubKeys.size)
          Left(HeaderWrongNumberOfCheckpointSignatures(checkpoint.signatures.size))
        else if (invalidSignatures.nonEmpty) {
          val sigsWithKeys = invalidSignatures.map { case (sig, maybePk) =>
            (sig, maybePk.map(ByteStringUtils.hash2string))
          }
          Left(HeaderInvalidCheckpointSignatures(sigsWithKeys))
        } else if (validSignatures.size < blockchainConfig.minRequireSignatures)
          Left(HeaderWrongNumberOfCheckpointSignatures(validSignatures.size))
        else
          Right(BlockHeaderValid)
      }
      .getOrElse(Left(HeaderUnexpectedError("Attempted to validate a checkpoint on a block without a checkpoint")))
  }

  /**
    * Validates emptiness of:
    * - beneficiary
    * - difficulty
    * - extraData
    * - treasuryOptOut
    * - ommersHash
    * - transactionsRoot
    * - receiptsRoot
    * - logsBloom
    * - nonce
    * - mixHash
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderFieldNotEmptyError]] otherwise
    */
  private def validateEmptyFields(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    if (blockHeader.beneficiary != BlockHeader.EmptyBeneficiary)
      notEmptyFieldError("beneficiary")
    else if (blockHeader.difficulty != BlockHeader.EmptyDifficulty)
      notEmptyFieldError("difficulty")
    else if (blockHeader.ommersHash != BlockHeader.EmptyOmmers)
      notEmptyFieldError("ommersHash")
    else if (blockHeader.transactionsRoot != BlockHeader.EmptyMpt)
      notEmptyFieldError("transactionsRoot")
    else if (blockHeader.receiptsRoot != BlockHeader.EmptyMpt)
      notEmptyFieldError("receiptsRoot")
    else if (blockHeader.logsBloom != BloomFilter.EmptyBloomFilter)
      notEmptyFieldError("logsBloom")
    else if (blockHeader.extraData.nonEmpty)
      notEmptyFieldError("extraData")
    else if (blockHeader.nonce.nonEmpty)
      notEmptyFieldError("nonce")
    else if (blockHeader.mixHash.nonEmpty)
      notEmptyFieldError("mixHash")
    else Right(BlockHeaderValid)
  }

  private def notEmptyFieldError(field: String) = Left(HeaderFieldNotEmptyError(s"$field is not empty"))

  /**
    * Validates fields which should be equal to parent equivalents:
    * - stateRoot
    * - gasLimit
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderNotMatchParentError]] otherwise
    */
  private def validateFieldsCopiedFromParent(
      blockHeader: BlockHeader,
      parentHeader: BlockHeader
  ): Either[BlockHeaderError, BlockHeaderValid] = {
    if (blockHeader.stateRoot != parentHeader.stateRoot)
      fieldNotMatchedParentFieldError("stateRoot")
    else if (blockHeader.gasLimit != parentHeader.gasLimit)
      fieldNotMatchedParentFieldError("gasLimit")
    else Right(BlockHeaderValid)
  }

  private def fieldNotMatchedParentFieldError(field: String) =
    Left(HeaderNotMatchParentError(s"$field has different value that similar parent field"))

  /**
    * Validates gasUsed equal to zero
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderGasUsedError]] otherwise
    */
  private def validateGasUsed(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    if (blockHeader.gasUsed != BigInt(0)) Left(HeaderGasUsedError)
    else Right(BlockHeaderValid)
  }

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.unixTimestamp]] is one bigger than parent unixTimestamp
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderTimestampError]] otherwise
    */
  private def validateTimestamp(
      blockHeader: BlockHeader,
      parentHeader: BlockHeader
  ): Either[BlockHeaderError, BlockHeaderValid] =
    if (blockHeader.unixTimestamp == parentHeader.unixTimestamp + 1) Right(BlockHeaderValid)
    else Left(HeaderTimestampError)

  private def validateTreasuryOptOut(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    if (blockHeader.treasuryOptOut.contains(false)) Right(BlockHeaderValid)
    else Left(CheckpointHeaderTreasuryOptOutError)

}
