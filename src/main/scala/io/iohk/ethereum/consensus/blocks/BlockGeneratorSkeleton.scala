package io.iohk.ethereum.consensus.blocks

import java.util.concurrent.atomic.AtomicReference

import akka.util.ByteString

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.mining.MiningConfig
import io.iohk.ethereum.consensus.pow.blocks.Ommers
import io.iohk.ethereum.consensus.pow.blocks.OmmersSeqEnc
import io.iohk.ethereum.consensus.validators.std.MptListValidator.intByteArraySerializable
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.BlockResult
import io.iohk.ethereum.ledger.BloomFilter
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.PreparedBlock
import io.iohk.ethereum.mpt.ByteArraySerializable
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteUtils.or

/** This is a skeleton for a generic [[io.iohk.ethereum.consensus.blocks.BlockGenerator BlockGenerator]].
  */
abstract class BlockGeneratorSkeleton(
    miningConfig: MiningConfig,
    difficultyCalc: DifficultyCalculator,
    _blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends TestBlockGenerator {

  protected val headerExtraData = miningConfig.headerExtraData

  protected val blockCacheSize = miningConfig.blockCacheSize

  protected val cache: AtomicReference[List[PendingBlockAndState]] = new AtomicReference(Nil)

  protected def newBlockBody(transactions: Seq[SignedTransaction], x: X): BlockBody

  protected def defaultPrepareHeader(
      blockNumber: BigInt,
      parent: Block,
      beneficiary: Address,
      blockTimestamp: Long,
      x: Ommers
  )(implicit blockchainConfig: BlockchainConfig): BlockHeader = {
    val extraFields =
      if (blockNumber >= blockchainConfig.forkBlockNumbers.ecip1097BlockNumber)
        HefPostEcip1097(None)
      else
        HefEmpty

    BlockHeader(
      parentHash = parent.header.hash,
      ommersHash = ByteString(kec256(x.toBytes: Array[Byte])),
      beneficiary = beneficiary.bytes,
      stateRoot = ByteString.empty,
      //we are not able to calculate transactionsRoot here because we do not know if they will fail
      transactionsRoot = ByteString.empty,
      receiptsRoot = ByteString.empty,
      logsBloom = ByteString.empty,
      difficulty = difficultyCalc.calculateDifficulty(blockNumber, blockTimestamp, parent.header),
      number = blockNumber,
      gasLimit = calculateGasLimit(parent.header.gasLimit),
      gasUsed = 0,
      unixTimestamp = blockTimestamp,
      extraData = blockchainConfig.daoForkConfig
        .flatMap(daoForkConfig => daoForkConfig.getExtraData(blockNumber))
        .getOrElse(headerExtraData),
      mixHash = ByteString.empty,
      nonce = ByteString.empty,
      extraFields = extraFields
    )
  }

  protected def prepareHeader(
      blockNumber: BigInt,
      parent: Block,
      beneficiary: Address,
      blockTimestamp: Long,
      x: X
  )(implicit blockchainConfig: BlockchainConfig): BlockHeader

  // scalastyle:off parameter.number
  protected def prepareBlock(
      evmCodeStorage: EvmCodeStorage,
      parent: Block,
      transactions: Seq[SignedTransaction],
      beneficiary: Address,
      blockNumber: BigInt,
      blockPreparator: BlockPreparator,
      x: X,
      initialWorldStateBeforeExecution: Option[InMemoryWorldStateProxy]
  )(implicit blockchainConfig: BlockchainConfig): PendingBlockAndState = {

    val blockTimestamp = blockTimestampProvider.getEpochSecond
    val header = prepareHeader(blockNumber, parent, beneficiary, blockTimestamp, x)
    val transactionsForBlock = prepareTransactions(transactions, header.gasLimit)
    val body = newBlockBody(transactionsForBlock, x)
    val block = Block(header, body)

    blockPreparator.prepareBlock(evmCodeStorage, block, parent.header, initialWorldStateBeforeExecution) match {
      case PreparedBlock(prepareBlock, BlockResult(_, gasUsed, receipts), stateRoot, updatedWorld) =>
        val receiptsLogs: Seq[Array[Byte]] =
          BloomFilter.EmptyBloomFilter.toArray +: receipts.map(_.logsBloomFilter.toArray)
        val bloomFilter = ByteString(or(receiptsLogs: _*))

        PendingBlockAndState(
          PendingBlock(
            block.copy(
              header = block.header.copy(
                transactionsRoot = buildMpt(prepareBlock.body.transactionList, SignedTransaction.byteArraySerializable),
                stateRoot = stateRoot,
                receiptsRoot = buildMpt(receipts, Receipt.byteArraySerializable),
                logsBloom = bloomFilter,
                gasUsed = gasUsed
              ),
              body = prepareBlock.body
            ),
            receipts
          ),
          updatedWorld
        )
    }
  }

  protected def prepareTransactions(
      transactions: Seq[SignedTransaction],
      blockGasLimit: BigInt
  )(implicit blockchainConfig: BlockchainConfig): Seq[SignedTransaction] = {

    val sortedTransactions: Seq[SignedTransaction] = transactions
      //should be safe to call get as we do not insert improper transactions to pool.
      .groupBy(tx => SignedTransaction.getSender(tx).get)
      .values
      .toList
      .flatMap { txsFromSender =>
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
      }
      .sortBy { case (gasPrice, _) => gasPrice }
      .reverse
      .flatMap { case (_, txs) => txs }

    val transactionsForBlock: Seq[SignedTransaction] = sortedTransactions
      .scanLeft((BigInt(0), None: Option[SignedTransaction])) { case ((accumulatedGas, _), stx) =>
        (accumulatedGas + stx.tx.gasLimit, Some(stx))
      }
      .collect { case (gas, Some(stx)) => (gas, stx) }
      .takeWhile { case (gas, _) => gas <= blockGasLimit }
      .map { case (_, stx) => stx }

    transactionsForBlock
  }

  /*
        Returns the same gas limit as the parent block

        In Mantis only testnets (and without this changed), this means that all blocks will have the same gasLimit as
        the genesis block
   */
  protected def calculateGasLimit(parentGas: BigInt): BigInt = parentGas

  protected def buildMpt[K](entities: Seq[K], vSerializable: ByteArraySerializable[K]): ByteString = {
    val stateStorage = StateStorage.getReadOnlyStorage(EphemDataSource())
    val mpt = MerklePatriciaTrie[Int, K](
      source = stateStorage
    )(intByteArraySerializable, vSerializable)
    val hash = entities.zipWithIndex.foldLeft(mpt) { case (trie, (value, key)) => trie.put(key, value) }.getRootHash
    ByteString(hash)
  }

  def blockTimestampProvider: BlockTimestampProvider = _blockTimestampProvider

  /** This function returns the block currently being mined block with highest timestamp
    */
  def getPendingBlock: Option[PendingBlock] =
    getPendingBlockAndState.map(_.pendingBlock)

  def getPendingBlockAndState: Option[PendingBlockAndState] = {
    val pendingBlocks = cache.get()
    if (pendingBlocks.isEmpty) None
    else Some(pendingBlocks.maxBy(_.pendingBlock.block.header.unixTimestamp))
  }
}
