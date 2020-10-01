package io.iohk.ethereum.checkpointing

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BloomFilter
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

object CheckpointingTestHelpers {
  def createBlockWithCheckpoint(
    parentHeader: BlockHeader,
    checkpoint: Checkpoint
  ): Block = {
    Block(createBlockHeaderWithCheckpoint(parentHeader, checkpoint), BlockBody(Nil, Nil))
  }

  def createBlockHeaderWithCheckpoint(
    parentHeader: BlockHeader,
    checkpoint: Checkpoint
  ): BlockHeader = {
    BlockHeader.buildPostECIP1097Header(
      parentHash = parentHeader.hash,
      beneficiary = BlockHeader.EmptyBeneficiary,
      stateRoot = parentHeader.stateRoot,
      ommersHash = BlockHeader.EmptyOmmers,
      transactionsRoot = BlockHeader.EmptyMpt,
      receiptsRoot = BlockHeader.EmptyMpt,
      logsBloom = BloomFilter.EmptyBloomFilter,
      difficulty = parentHeader.difficulty,
      number = parentHeader.number + 1,
      gasLimit = parentHeader.gasLimit,
      gasUsed = UInt256.Zero,
      unixTimestamp = parentHeader.unixTimestamp + 1,
      extraData = ByteString.empty,
      mixHash = ByteString.empty,
      nonce = ByteString.empty,
      treasuryOptOut = false,
      checkpoint = Some(checkpoint)
    )
  }

  def createCheckpointSignatures(
    keys: Seq[AsymmetricCipherKeyPair],
    hash: ByteString
  ): Seq[ECDSASignature] =
    keys.map { k =>
      ECDSASignature.sign(hash.toArray, k)
    }
}
