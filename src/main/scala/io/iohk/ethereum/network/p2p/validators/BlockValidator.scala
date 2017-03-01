package io.iohk.ethereum.network.p2p.validators

import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.{Block, BlockHeader, SignedTransaction}
import io.iohk.ethereum.mpt.RLPByteArraySerializable
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV63.Receipt
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

object BlockValidator {
  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.transactionsRoot]] matches [[BlockBody.transactionList]]
    * based on validations stated in section 4.2.2 of http://paper.gavwood.com/
    *
    * @param block Block to validate
    * @return Block if valid, a Some otherwise
    */
  private def validateTransactionRoot(block: Block): Either[BlockError, Block] = {
    val isValid = MptListValidator.isValid[SignedTransaction](block.header.transactionsRoot.toArray[Byte],
      block.body.transactionList,
      new RLPByteArraySerializable[SignedTransaction]
    )
    if (isValid) Right(block)
    else Left(BlockTransactionsHashError)
  }

  /**
    * Validates [[BlockBody.uncleNodesList]] against [[io.iohk.ethereum.domain.BlockHeader.ommersHash]]
    * based on validations stated in section 4.2.2 of http://paper.gavwood.com/
    *
    * @param block Block to validate
    * @return Block if valid, a Some otherwise
    */
  private def validateOmmers(block: Block): Either[BlockError, Block] = {
    // FIXME Can we avoid encoding ommers again?
    val encodedOmmers = encode(BlockBody.encodeUncles(block.body.uncleNodesList))
    if (kec256(encodedOmmers) sameElements block.header.ommersHash) Right(block)
    else Left(BlockOmmersHashError)
  }

  /**
    * Validates [[Receipt]] against [[io.iohk.ethereum.domain.BlockHeader.receiptsRoot]]
    * based on validations stated in section 4.2.2 of http://paper.gavwood.com/
    *
    * @param block    Block to validate
    * @param receipts Receipts to use
    * @return
    */
  private def validateReceipts(block: Block, receipts: Seq[Receipt]): Either[BlockError, Block] = {

    val isValid = MptListValidator.isValid[Receipt](block.header.receiptsRoot.toArray[Byte],
      receipts,
      new RLPByteArraySerializable[Receipt]
    )
    if (isValid) Right(block)
    else Left(BlockReceiptsHashError)
  }

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.logsBloom]] against [[Receipt.logsBloomFilter]]
    * based on validations stated in section 4.2.2 of http://paper.gavwood.com/
    *
    * @param block    Block to validate
    * @param receipts Receipts to use
    * @return
    */
  private def validateLogBloom(block: Block, receipts: Seq[Receipt]): Either[BlockError, Block] = {
    val logsBloomOr = receipts.foldLeft(Array.fill(block.header.logsBloom.size)(0.toByte)) { (or: Array[Byte], receipt: Receipt) =>
      or.zip(receipt.logsBloomFilter.toArray[Byte]).map(b => (b._1 | b._2).toByte)
    }
    if (logsBloomOr sameElements block.header.logsBloom.toArray[Byte]) Right(block)
    else Left(BlockLogBloomError)
  }

  /**
    * This method allows validate a Block. It only perfoms the following validations (stated on
    * section 4.2.2 of http://paper.gavwood.com/):
    *   - [[BlockValidator.validateTransactionRoot]]
    *   - [[BlockValidator.validateOmmers]]
    *   - [[BlockValidator.validateReceipts]]
    *   - [[BlockValidator.validateLogBloom]]
    *
    * @param block  Block to validate
    * @param receipts Receipts to be in validation process
    * @return The block if validations are ok, error otherwise
    */
  def validate(block: Block, receipts: Seq[Receipt]): Either[BlockError, Block] = {
    for {
      _ <- validateHeaderAndBody(block.header, block.body)
      _ <- validateReceipts(block, receipts)
      _ <- validateLogBloom(block, receipts)
    } yield block
  }

  /**
    * This method allows validate that a BlockHeader matches a BlockBody. It only perfoms the following validations (stated on
    * section 4.2.2 of http://paper.gavwood.com/):
    *   - [[BlockValidator.validateTransactionRoot]]
    *   - [[BlockValidator.validateOmmers]]
    *
    * @param blockHeader to validate
    * @param blockBody to validate
    * @return The block if the header matched the body, error otherwise
    */
  def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody): Either[BlockError, Block] = {
    val block = Block(blockHeader, blockBody)
    for {
      _ <- validateTransactionRoot(block)
      _ <- validateOmmers(block)
    } yield block
  }

  sealed trait BlockError

  case object BlockTransactionsHashError extends BlockError

  case object BlockOmmersHashError extends BlockError

  case object BlockReceiptsHashError extends BlockError

  case object BlockLogBloomError extends BlockError

}
