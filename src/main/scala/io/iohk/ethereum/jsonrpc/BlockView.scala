package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.json4s.CustomSerializer
import org.json4s.JsonAST._
import org.spongycastle.util.encoders.Hex

case class BlockView(
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
                      size: Long,
                      gasLimit: BigInt,
                      gasUsed: BigInt,
                      timestamp: Long,
                      transactions: Either[Seq[ByteString], Seq[SignedTransactionView]],
                      unclesHash: Seq[ByteString]
                    )

object BlockView {

  def apply(block: Block, txHashed: Boolean = false, totalDifficulty: Option[BigInt] = None): BlockView = {
    val transactions =
      if (txHashed)
        Left(block.body.transactionList.map(_.hash))
      else
        Right(block.body.transactionList.zipWithIndex.map { case (stx, transactionIndex) =>
          SignedTransactionView(stx = stx, block = Some(block), transactionIndex = Some(transactionIndex))
        })

    BlockView(
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
      unclesHash = block.body.uncleNodesList.map(_.hash)
    )
  }

  def apply(blockHeader: BlockHeader, totalDifficulty: Option[BigInt]): BlockView =
    BlockView(
      block = Block(blockHeader, BlockBody(Nil, Nil)),
      totalDifficulty = totalDifficulty
    )

  //FIXME: Add remaining fields and move from here
  def jsonEncode(blockView: BlockView): JValue = {
     JObject(List(
       JField("hash", blockView.hash.map(h => JString(Hex.toHexString(h.toArray))).getOrElse(JNull)),
       JField("parentHash", JString(Hex.toHexString(blockView.parentHash.toArray)))
     ))
  }

}
