package io.iohk.ethereum.consensus
package ethash

import java.util.function.UnaryOperator

import akka.util.ByteString
import io.iohk.ethereum.consensus.BlockGenerator.InvalidOmmers
import io.iohk.ethereum.consensus.ethash.validators.EthashValidators
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.Ledger.{BlockPreparationResult, BlockResult}
import io.iohk.ethereum.ledger.{BlockPreparationError, BloomFilter, Ledger}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteUtils.or



//
class EthashBlockGenerator(
  validators: EthashValidators,
  blockchain: Blockchain,
  blockchainConfig: BlockchainConfig,
  headerExtraData: ByteString,
  blockCacheSize: Int,
  ledger: Ledger,
  val blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends BlockGenerator[Seq[BlockHeader]] {

  private[this] final val difficulty = new DifficultyCalculator(blockchainConfig)

  //returns maximal limit to be able to include as many transactions as possible
  private def calculateGasLimit(parentGas: BigInt): BigInt = {
    val GasLimitBoundDivisor: Int = 1024

    val gasLimitDifference = parentGas / GasLimitBoundDivisor
    parentGas + gasLimitDifference - 1
  }


  def prepareHeader(
    blockNumber: BigInt, parent: Block,
    beneficiary: Address, blockTimestamp: Long,
    ommers: Seq[BlockHeader]
  ): BlockHeader = {

    import blockchainConfig.daoForkConfig

    BlockHeader(
      parentHash = parent.header.hash,
      ommersHash = ByteString(kec256(ommers.toBytes: Array[Byte])),
      beneficiary = beneficiary.bytes,
      stateRoot = ByteString.empty,
      //we are not able to calculate transactionsRoot here because we do not know if they will fail
      transactionsRoot = ByteString.empty,
      receiptsRoot = ByteString.empty,
      logsBloom = ByteString.empty,
      difficulty = difficulty.calculateDifficulty(blockNumber, blockTimestamp, parent.header),
      number = blockNumber,
      gasLimit = calculateGasLimit(parent.header.gasLimit),
      gasUsed = 0,
      unixTimestamp = blockTimestamp,
      extraData = daoForkConfig.flatMap(daoForkConfig => daoForkConfig.getExtraData(blockNumber)).getOrElse(headerExtraData),
      mixHash = ByteString.empty,
      nonce = ByteString.empty
    )
  }

  def generateBlockForMining(
    parent: Block,
    transactions: Seq[SignedTransaction],
    beneficiary: Address,
    ommers: Seq[BlockHeader]
  ): Either[BlockPreparationError, PendingBlock] = {

    val blockNumber = parent.header.number + 1
    val parentHash = parent.header.hash

    val result: Either[InvalidOmmers, PendingBlockAndState] = validators.ommersValidator
      .validate(parentHash, blockNumber, ommers, blockchain)
      .left.map(InvalidOmmers).flatMap { _ =>
      val blockTimestamp = blockTimestampProvider.getEpochSecond
      val header: BlockHeader = prepareHeader(blockNumber, ommers, beneficiary, parent, blockTimestamp)
      val transactionsForBlock: List[SignedTransaction] = prepareTransactions(transactions, header.gasLimit)
      val body = BlockBody(transactionsForBlock, ommers)
      val block = Block(header, body)

      val prepared = ledger.prepareBlock(block) match {
        case BlockPreparationResult(prepareBlock, BlockResult(_, gasUsed, receipts), stateRoot, updatedWorld) =>
          val receiptsLogs: Seq[Array[Byte]] = BloomFilter.EmptyBloomFilter.toArray +: receipts.map(_.logsBloomFilter.toArray)
          val bloomFilter = ByteString(or(receiptsLogs: _*))

          PendingBlockAndState(PendingBlock(block.copy(header = block.header.copy(
            transactionsRoot = buildMpt(prepareBlock.body.transactionList, SignedTransaction.byteArraySerializable),
            stateRoot = stateRoot,
            receiptsRoot = buildMpt(receipts, Receipt.byteArraySerializable),
            logsBloom = bloomFilter,
            gasUsed = gasUsed),
            body = prepareBlock.body), receipts), updatedWorld)
      }
      Right(prepared)
    }

    result.right.foreach(b => cache.updateAndGet(new UnaryOperator[List[PendingBlockAndState]] {
      override def apply(t: List[PendingBlockAndState]): List[PendingBlockAndState] =
        (b :: t).take(blockCacheSize)
    }))

    result.map(_.pendingBlock)
  }
}
