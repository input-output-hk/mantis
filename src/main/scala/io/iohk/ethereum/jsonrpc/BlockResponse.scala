package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

case class BlockResponse(
                          number: Option[BigInt],
                          hash: Option[ByteString],
                          parentHash: ByteString,
                          nonce: Option[ByteString],
                          sha3Uncles: ByteString,
                          logsBloom: ByteString,
                          transactionsRoot: ByteString,
                          stateRoot: ByteString,
                          receiptsRoot: ByteString,
                          miner: ByteString,
                          difficulty: BigInt,
                          totalDifficulty: Option[BigInt],
                          extraData: ByteString,
                          size: BigInt,
                          gasLimit: BigInt,
                          gasUsed: BigInt,
                          timestamp: BigInt,
                          transactions: Either[Seq[ByteString], Seq[TransactionResponse]],
                          uncles: Seq[ByteString]
                        )

object BlockResponse {

  def apply(block: Block, txHashed: Boolean = false, totalDifficulty: Option[BigInt] = None): BlockResponse = {
    val transactions =
      if (txHashed)
        Left(block.body.transactionList.map(_.hash))
      else
        Right(block.body.transactionList.zipWithIndex.map { case (stx, transactionIndex) =>
          TransactionResponse(stx = stx, blockHeader = Some(block.header), transactionIndex = Some(transactionIndex))
        })

    BlockResponse(
      number = Some(block.header.number),
      hash = Some(block.header.hash),
      parentHash = block.header.parentHash,
      nonce = Some(block.header.nonce),
      sha3Uncles = block.header.ommersHash,
      logsBloom = block.header.logsBloom,
      transactionsRoot = block.header.transactionsRoot,
      stateRoot = block.header.stateRoot,
      receiptsRoot = block.header.receiptsRoot,
      miner = block.header.beneficiary,
      difficulty = block.header.difficulty,
      totalDifficulty = totalDifficulty,
      extraData = block.header.extraData,
      size = Block.size(block),
      gasLimit = block.header.gasLimit,
      gasUsed = block.header.gasUsed,
      timestamp = block.header.unixTimestamp,
      transactions = transactions,
      uncles = block.body.uncleNodesList.map(_.hash)
    )
  }

  def apply(blockHeader: BlockHeader, totalDifficulty: Option[BigInt]): BlockResponse =
    BlockResponse(
      block = Block(blockHeader, BlockBody(Nil, Nil)),
      totalDifficulty = totalDifficulty
    )

}
