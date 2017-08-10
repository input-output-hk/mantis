package io.iohk.ethereum.mining

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.{ArchiveNodeStorage, NodeStorage}
import io.iohk.ethereum.domain.{Block, BlockHeader, Receipt, SignedTransaction, _}
import io.iohk.ethereum.ledger.Ledger.{BlockPreparationResult, BlockResult}
import io.iohk.ethereum.ledger.{BlockPreparationError, BloomFilter, Ledger}
import io.iohk.ethereum.mining.BlockGenerator.{InvalidOmmers, NoParent}
import io.iohk.ethereum.mpt.{ByteArraySerializable, MerklePatriciaTrie}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
import io.iohk.ethereum.utils.{BlockchainConfig, MiningConfig}
import io.iohk.ethereum.utils.ByteUtils.or
import io.iohk.ethereum.validators.MptListValidator.intByteArraySerializable
import io.iohk.ethereum.validators.OmmersValidator.OmmersError
import io.iohk.ethereum.validators.Validators
import io.iohk.ethereum.crypto._

class BlockGenerator(blockchain: BlockchainImpl, blockchainConfig: BlockchainConfig, miningConfig: MiningConfig,
  ledger: Ledger, validators: Validators, blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider) {

  val difficulty = new DifficultyCalculator(blockchainConfig)

  private val cache: AtomicReference[List[PendingBlock]] = new AtomicReference(Nil)

  def generateBlockForMining(blockNumber: BigInt, transactions: Seq[SignedTransaction], ommers: Seq[BlockHeader], beneficiary: Address):
  Either[BlockPreparationError, PendingBlock] = {

    val result = validators.ommersValidator.validate(blockNumber, ommers, blockchain).left.map(InvalidOmmers).flatMap { _ =>
      blockchain.getBlockByNumber(blockNumber - 1).map { parent =>
        val blockTimestamp = blockTimestampProvider.getEpochSecond
        val header: BlockHeader = prepareHeader(blockNumber, ommers, beneficiary, parent, blockTimestamp)
        val transactionsForBlock: List[SignedTransaction] = prepareTransactions(transactions, header.gasLimit)
        val body = BlockBody(transactionsForBlock, ommers)
        val block = Block(header, body)

        val prepared = ledger.prepareBlock(block, blockchain, validators) match {
          case BlockPreparationResult(prepareBlock, BlockResult(_, gasUsed, receipts), stateRoot) =>
            val receiptsLogs: Seq[Array[Byte]] = BloomFilter.EmptyBloomFilter.toArray +: receipts.map(_.logsBloomFilter.toArray)
            val bloomFilter = ByteString(or(receiptsLogs: _*))

            PendingBlock(block.copy(header = block.header.copy(
              transactionsRoot = buildMpt(prepareBlock.body.transactionList, SignedTransaction.byteArraySerializable),
              stateRoot = stateRoot,
              receiptsRoot = buildMpt(receipts, Receipt.byteArraySerializable),
              logsBloom = bloomFilter,
              gasUsed = gasUsed),
              body = prepareBlock.body), receipts)
        }
        Right(prepared)
      }.getOrElse(Left(NoParent))
    }

    result.right.foreach(b => cache.updateAndGet(new UnaryOperator[List[PendingBlock]] {
      override def apply(t: List[PendingBlock]): List[PendingBlock] =
        (b :: t).take(miningConfig.blockCacheSize)
    }))

    result
  }

  private def prepareTransactions(transactions: Seq[SignedTransaction], blockGasLimit: BigInt) = {
    val sortedTransactions = transactions.groupBy(_.senderAddress).values.toList.flatMap { txsFromSender =>
      val ordered = txsFromSender
        .sortBy(-_.tx.gasPrice)
        .sortBy(_.tx.nonce)
        .foldLeft(Seq.empty[SignedTransaction]) { case (txs, tx) =>
          if (txs.exists(_.tx.nonce == tx.tx.nonce)) {
            txs
          } else {
            txs :+ tx
          }
        }
        .takeWhile(_.tx.gasLimit <= blockGasLimit)
      ordered.headOption.map(_.tx.gasPrice -> ordered)
    }.sortBy { case (gasPrice, _) => gasPrice }.reverse.flatMap { case (_, txs) => txs }

    val transactionsForBlock = sortedTransactions
      .scanLeft(BigInt(0), None: Option[SignedTransaction]) { case ((accumulatedGas, _), stx) => (accumulatedGas + stx.tx.gasLimit, Some(stx)) }
      .collect { case (gas, Some(stx)) => (gas, stx) }
      .takeWhile { case (gas, _) => gas <= blockGasLimit }
      .map { case (_, stx) => stx }
    transactionsForBlock
  }

  private def prepareHeader(blockNumber: BigInt, ommers: Seq[BlockHeader], beneficiary: Address, parent: Block, blockTimestamp: Long) = BlockHeader(
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
    extraData = ByteString("mined with etc scala"),
    mixHash = ByteString.empty,
    nonce = ByteString.empty
  )

  def getPrepared(powHeaderHash: ByteString): Option[PendingBlock] = {
    cache.getAndUpdate(new UnaryOperator[List[PendingBlock]] {
      override def apply(t: List[PendingBlock]): List[PendingBlock] =
        t.filterNot(pb => ByteString(kec256(BlockHeader.getEncodedWithoutNonce(pb.block.header))) == powHeaderHash)
    }).find { pb =>
      ByteString(kec256(BlockHeader.getEncodedWithoutNonce(pb.block.header))) == powHeaderHash
    }
  }

  /**
    * This function returns the block currently being mined block with highest timestamp
    */
  def getPending: Option[PendingBlock] = {
    val pendingBlocks = cache.get()
    if(pendingBlocks.isEmpty) None
    else Some(pendingBlocks.maxBy(_.block.header.unixTimestamp))
  }

  //returns maximal limit to be able to include as many transactions as possible
  private def calculateGasLimit(parentGas: BigInt): BigInt = {
    val GasLimitBoundDivisor: Int = 1024

    val gasLimitDifference = parentGas / GasLimitBoundDivisor
    parentGas + gasLimitDifference - 1
  }

  private def buildMpt[K](entities: Seq[K], vSerializable: ByteArraySerializable[K]): ByteString = {
    val mpt = MerklePatriciaTrie[Int, K](
      source = new ArchiveNodeStorage(new NodeStorage(EphemDataSource())),
      hashFn = (input: Array[Byte]) => kec256(input)
    )(intByteArraySerializable, vSerializable)
    val hash = entities.zipWithIndex.foldLeft(mpt) { case (trie, (value, key)) => trie.put(key, value) }.getRootHash
    ByteString(hash)
  }

}

trait BlockTimestampProvider {
  def getEpochSecond: Long
}

case class PendingBlock(block: Block, receipts: Seq[Receipt])

object DefaultBlockTimestampProvider extends BlockTimestampProvider {
  override def getEpochSecond: Long = Instant.now.getEpochSecond
}

object BlockGenerator {

  case object NoParent extends BlockPreparationError

  case class InvalidOmmers(reason: OmmersError) extends BlockPreparationError

}
