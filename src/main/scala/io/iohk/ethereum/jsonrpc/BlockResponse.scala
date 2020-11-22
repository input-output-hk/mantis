package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import cats.implicits._
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Block, BlockBody, BlockHeader, ChainWeight}

case class CheckpointResponse(signatures: Seq[ECDSASignature], signers: Seq[ByteString])

case class BlockResponse(
    number: BigInt,
    hash: Option[ByteString],
    parentHash: ByteString,
    nonce: Option[ByteString],
    sha3Uncles: ByteString,
    logsBloom: ByteString,
    transactionsRoot: ByteString,
    stateRoot: ByteString,
    receiptsRoot: ByteString,
    miner: Option[ByteString],
    difficulty: BigInt,
    totalDifficulty: Option[BigInt],
    lastCheckpointNumber: Option[BigInt],
    extraData: ByteString,
    size: BigInt,
    gasLimit: BigInt,
    gasUsed: BigInt,
    timestamp: BigInt,
    checkpoint: Option[CheckpointResponse],
    treasuryOptOut: Option[Boolean],
    transactions: Either[Seq[ByteString], Seq[TransactionResponse]],
    uncles: Seq[ByteString]
) {
  val chainWeight: Option[ChainWeight] = for {
    lcn <- lastCheckpointNumber
    td <- totalDifficulty
  } yield ChainWeight(lcn, td)
}

object BlockResponse {

  def apply(
      block: Block,
      weight: Option[ChainWeight] = None,
      fullTxs: Boolean = false,
      pendingBlock: Boolean = false
  ): BlockResponse = {
    val transactions =
      if (fullTxs) {
        val txnData = block.body.enumerate.toStream.map { case (stx, transactionIndex) =>
          TransactionResponse(stx = stx, blockHeader = Some(block.header), transactionIndex = Some(transactionIndex))
        }
        Right(txnData)
      } else
        Left(block.body.iterator.toStream.map(_.hash))

    val checkpoint = block.header.checkpoint.map { checkpoint =>
      val signers = checkpoint.signatures.flatMap(_.publicKey(block.header.parentHash))

      CheckpointResponse(checkpoint.signatures, signers)
    }

    val (lcn, td) = weight.map(_.asTuple).separate

    BlockResponse(
      number = block.header.number,
      hash = if (pendingBlock) None else Some(block.header.hash),
      parentHash = block.header.parentHash,
      nonce = if (pendingBlock) None else Some(block.header.nonce),
      sha3Uncles = block.header.ommersHash,
      logsBloom = block.header.logsBloom,
      transactionsRoot = block.header.transactionsRoot,
      stateRoot = block.header.stateRoot,
      receiptsRoot = block.header.receiptsRoot,
      miner = if (pendingBlock) None else Some(block.header.beneficiary),
      difficulty = block.header.difficulty,
      totalDifficulty = td,
      lastCheckpointNumber = lcn,
      extraData = block.header.extraData,
      size = Block.size(block),
      gasLimit = block.header.gasLimit,
      gasUsed = block.header.gasUsed,
      timestamp = block.header.unixTimestamp,
      checkpoint = checkpoint,
      treasuryOptOut = block.header.treasuryOptOut,
      transactions = transactions,
      uncles = block.body.uncleNodesList.map(_.hash)
    )
  }

  def apply(blockHeader: BlockHeader, weight: Option[ChainWeight], pendingBlock: Boolean): BlockResponse =
    BlockResponse(
      block = Block(blockHeader, BlockBody(Nil, Nil)),
      weight = weight,
      pendingBlock = pendingBlock
    )

}
