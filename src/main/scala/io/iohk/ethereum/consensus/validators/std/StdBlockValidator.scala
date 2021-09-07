package io.iohk.ethereum.consensus.validators.std

import akka.util.ByteString

import io.iohk.ethereum.consensus.pow.blocks.OmmersSeqEnc
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.ledger.BloomFilter
import io.iohk.ethereum.utils.ByteUtils.or

object StdBlockValidator extends BlockValidator {

  def transactionsRootHash(blockBody: BlockBody): Array[Byte] =
    MptListValidator.rootHash(blockBody.transactionList, SignedTransaction.byteArraySerializable)

  /** Validates [[io.iohk.ethereum.domain.BlockHeader.transactionsRoot]] matches [[BlockBody.transactionList]]
    * based on validations stated in section 4.4.2 of http://paper.gavwood.com/
    *
    * @param block Block to validate
    * @return Block if valid, a Some otherwise
    */
  private def validateTransactionRoot(block: Block): Either[BlockError, BlockValid] =
    if (transactionsRootHash(block.body).sameElements(block.header.transactionsRoot.toArray[Byte]))
      Right(BlockValid)
    else
      Left(BlockTransactionsHashError)

  def blockBodyOmmersHash(blockBody: BlockBody): Array[Byte] =
    kec256(blockBody.uncleNodesList.toBytes)

  /** Validates [[BlockBody.uncleNodesList]] against [[io.iohk.ethereum.domain.BlockHeader.ommersHash]]
    * based on validations stated in section 4.4.2 of http://paper.gavwood.com/
    *
    * @param block Block to validate
    * @return Block if valid, a Some otherwise
    */
  private def validateOmmersHash(block: Block): Either[BlockError, BlockValid] =
    if (blockBodyOmmersHash(block.body).sameElements(block.header.ommersHash))
      Right(BlockValid)
    else
      Left(BlockOmmersHashError)

  /** Validates [[Receipt]] against [[io.iohk.ethereum.domain.BlockHeader.receiptsRoot]]
    * based on validations stated in section 4.4.2 of http://paper.gavwood.com/
    *
    * @param blockHeader    Block header to validate
    * @param receipts Receipts to use
    * @return
    */
  private def validateReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]): Either[BlockError, BlockValid] = {

    val isValid =
      MptListValidator.isValid[Receipt](blockHeader.receiptsRoot.toArray[Byte], receipts, Receipt.byteArraySerializable)
    if (isValid) Right(BlockValid)
    else Left(BlockReceiptsHashError)
  }

  /** Validates [[io.iohk.ethereum.domain.BlockHeader.logsBloom]] against [[Receipt.logsBloomFilter]]
    * based on validations stated in section 4.4.2 of http://paper.gavwood.com/
    *
    * @param blockHeader  Block header to validate
    * @param receipts     Receipts to use
    * @return
    */
  private def validateLogBloom(blockHeader: BlockHeader, receipts: Seq[Receipt]): Either[BlockError, BlockValid] = {
    val logsBloomOr =
      if (receipts.isEmpty) BloomFilter.EmptyBloomFilter
      else ByteString(or(receipts.map(_.logsBloomFilter.toArray): _*))
    if (logsBloomOr == blockHeader.logsBloom) Right(BlockValid)
    else Left(BlockLogBloomError)
  }

  /** Validates that the block body does not contain transactions
    *
    * @param blockBody BlockBody to validate
    * @return BlockValid if there are no transactions, error otherwise
    */
  private def validateNoTransactions(blockBody: BlockBody): Either[BlockError, BlockValid] =
    Either.cond(blockBody.transactionList.isEmpty, BlockValid, CheckpointBlockTransactionsNotEmptyError)

  /** Validates that the block body does not contain ommers
    *
    * @param blockBody BlockBody to validate
    * @return BlockValid if there are no ommers, error otherwise
    */
  private def validateNoOmmers(blockBody: BlockBody): Either[BlockError, BlockValid] =
    Either.cond(blockBody.uncleNodesList.isEmpty, BlockValid, CheckpointBlockOmmersNotEmptyError)

  /** This method allows validate block with checkpoint. It performs the following validations:
    *   - no transactions in the body
    *   - no ommers in the body
    *
    * @param blockBody BlockBody to validate
    * @return The BlockValid if validations are ok, BlockError otherwise
    */
  private def validateBlockWithCheckpoint(blockBody: BlockBody): Either[BlockError, BlockValid] =
    for {
      _ <- validateNoTransactions(blockBody)
      _ <- validateNoOmmers(blockBody)
    } yield BlockValid

  /** This method allows validate a regular Block. It only performs the following validations (stated on
    * section 4.4.2 of http://paper.gavwood.com/):
    *   - BlockValidator.validateTransactionRoot
    *   - BlockValidator.validateOmmersHash
    *
    * @param block Block to validate
    * @return The BlockValid if validations are ok, BlockError otherwise
    */
  private def validateRegularBlock(block: Block): Either[BlockError, BlockValid] =
    for {
      _ <- validateTransactionRoot(block)
      _ <- validateOmmersHash(block)
    } yield BlockValid

  /** This method allows validate a Block. It only perfoms the following validations (stated on
    * section 4.4.2 of http://paper.gavwood.com/):
    *   - validate regular block or block with checkpoint
    *   - BlockValidator.validateReceipts
    *   - BlockValidator.validateLogBloom
    *
    * @param block    Block to validate
    * @param receipts Receipts to be in validation process
    * @return The block if validations are ok, error otherwise
    */
  def validate(block: Block, receipts: Seq[Receipt]): Either[BlockError, BlockValid] =
    for {
      _ <- validateHeaderAndBody(block.header, block.body)
      _ <- validateBlockAndReceipts(block.header, receipts)
    } yield BlockValid

  /** This method allows validate that a BlockHeader matches a BlockBody.
    *
    * @param blockHeader to validate
    * @param blockBody to validate
    * @return The block if the header matched the body, error otherwise
    */
  def validateHeaderAndBody(blockHeader: BlockHeader, blockBody: BlockBody): Either[BlockError, BlockValid] = {
    val block = Block(blockHeader, blockBody)
    if (blockHeader.hasCheckpoint) validateBlockWithCheckpoint(blockBody)
    else validateRegularBlock(block)
  }

  /** This method allows validations of the block with its associated receipts.
    * It only perfoms the following validations (stated on section 4.4.2 of http://paper.gavwood.com/):
    *   - BlockValidator.validateReceipts
    *   - BlockValidator.validateLogBloom
    *
    * @param blockHeader    Block header to validate
    * @param receipts Receipts to be in validation process
    * @return The block if validations are ok, error otherwise
    */
  def validateBlockAndReceipts(blockHeader: BlockHeader, receipts: Seq[Receipt]): Either[BlockError, BlockValid] =
    for {
      _ <- validateReceipts(blockHeader, receipts)
      _ <- validateLogBloom(blockHeader, receipts)
    } yield BlockValid

  sealed trait BlockError

  case object BlockTransactionsHashError extends BlockError

  case object BlockOmmersHashError extends BlockError

  case object BlockReceiptsHashError extends BlockError

  case object BlockLogBloomError extends BlockError

  case object CheckpointBlockTransactionsNotEmptyError extends BlockError

  case object CheckpointBlockOmmersNotEmptyError extends BlockError

  sealed trait BlockValid

  case object BlockValid extends BlockValid
}
