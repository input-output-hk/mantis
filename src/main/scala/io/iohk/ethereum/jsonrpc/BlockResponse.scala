package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import cats.implicits._
import io.iohk.ethereum.consensus.pow.RestrictedPoWSigner
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Block, BlockBody, BlockHeader, ChainWeight}
import io.iohk.ethereum.utils.ByteStringUtils

case class CheckpointResponse(signatures: Seq[ECDSASignature], signers: Seq[ByteString])

trait BaseBlockResponse {
  def number: BigInt
  def hash: Option[ByteString]
  def mixHash: Option[ByteString]
  def parentHash: ByteString
  def nonce: Option[ByteString]
  def sha3Uncles: ByteString
  def logsBloom: ByteString
  def transactionsRoot: ByteString
  def stateRoot: ByteString
  def receiptsRoot: ByteString
  def miner: Option[ByteString]
  def difficulty: BigInt
  def totalDifficulty: Option[BigInt]
  def extraData: ByteString
  def size: BigInt
  def gasLimit: BigInt
  def gasUsed: BigInt
  def timestamp: BigInt
  def transactions: Either[Seq[ByteString], Seq[BaseTransactionResponse]]
  def uncles: Seq[ByteString]
}

case class EthBlockResponse(
    number: BigInt,
    hash: Option[ByteString],
    mixHash: Option[ByteString],
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
    extraData: ByteString,
    size: BigInt,
    gasLimit: BigInt,
    gasUsed: BigInt,
    timestamp: BigInt,
    transactions: Either[Seq[ByteString], Seq[BaseTransactionResponse]],
    uncles: Seq[ByteString]
) extends BaseBlockResponse

//scalastyle:off method.length
case class BlockResponse(
    number: BigInt,
    hash: Option[ByteString],
    mixHash: Option[ByteString],
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
    uncles: Seq[ByteString],
    signature: String,
    signer: String
) extends BaseBlockResponse {
  val chainWeight: Option[ChainWeight] = for {
    lcn <- lastCheckpointNumber
    td <- totalDifficulty
  } yield ChainWeight(lcn, td)
}

object BlockResponse {

  val NotAvailable = "N/A"

  def apply(
      block: Block,
      weight: Option[ChainWeight] = None,
      fullTxs: Boolean = false,
      pendingBlock: Boolean = false,
      coinbase: Option[ByteString] = None
  ): BlockResponse = {
    val transactions =
      if (fullTxs)
        Right(block.body.transactionList.zipWithIndex.map { case (stx, transactionIndex) =>
          TransactionResponse(stx = stx, blockHeader = Some(block.header), transactionIndex = Some(transactionIndex))
        })
      else
        Left(block.body.transactionList.map(_.hash))

    val checkpoint = block.header.checkpoint.map { checkpoint =>
      val signers = checkpoint.signatures.flatMap(_.publicKey(block.header.parentHash))

      CheckpointResponse(checkpoint.signatures, signers)
    }

    val (lcn, td) = weight.map(_.asTuple).separate

    val signature =
      if (block.header.extraData.length >= ECDSASignature.EncodedLength)
        ECDSASignature.fromBytes(block.header.extraData.takeRight(ECDSASignature.EncodedLength))
      else None

    val signatureStr = signature.map(_.toBytes).map(ByteStringUtils.hash2string).getOrElse(NotAvailable)
    val signerStr = signature
      .flatMap(_.publicKey(RestrictedPoWSigner.hashHeaderForSigning(block.header)))
      .map(ByteStringUtils.hash2string)
      .getOrElse(NotAvailable)

    BlockResponse(
      number = block.header.number,
      hash = if (pendingBlock) None else Some(block.header.hash),
      mixHash = if (block.header.mixHash.isEmpty) None else Some(block.header.mixHash),
      parentHash = block.header.parentHash,
      nonce = if (block.header.nonce.isEmpty) None else Some(block.header.nonce),
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
      uncles = block.body.uncleNodesList.map(_.hash),
      signature = signatureStr,
      signer = signerStr
    )
  }

  def apply(blockHeader: BlockHeader, weight: Option[ChainWeight], pendingBlock: Boolean): BlockResponse =
    BlockResponse(
      block = Block(blockHeader, BlockBody(Nil, Nil)),
      weight = weight,
      pendingBlock = pendingBlock
    )

}
