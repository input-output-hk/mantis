package io.iohk.ethereum.mining

import java.time.Instant

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.domain.{Block, BlockHeader, Receipt, SignedTransaction, _}
import io.iohk.ethereum.ledger.Ledger.BlockResult
import io.iohk.ethereum.ledger.{BlockPreparationError, BloomFilter, Ledger}
import io.iohk.ethereum.mining.BlockGenerator.NoParent
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, RLPByteArraySerializable}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits.headerRlpEncDec
import io.iohk.ethereum.network.p2p.messages.PV63.ReceiptImplicits.receiptRlpEncDec
import io.iohk.ethereum.rlp.RLPImplicitConversions.toRlpList
import io.iohk.ethereum.rlp.encode
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteUtils.or
import io.iohk.ethereum.validators.MptListValidator.intByteArraySerializable
import io.iohk.ethereum.validators.Validators

class BlockGenerator(blockchainStorages: BlockchainStorages, blockchainConfig: BlockchainConfig, ledger: Ledger, validators: Validators) {
  val difficulty = new DifficultyCalculator(blockchainConfig)

  //todo add transactions and ommers validation?
  def generateBlockForMining(blockNumber: BigInt, transactions: Seq[SignedTransaction], ommers: Seq[BlockHeader], beneficiary: Address):
  Either[BlockPreparationError, Block] = {
    val blockchain = BlockchainImpl(blockchainStorages)

    val blockTimestamp = Instant.now.getEpochSecond
    blockchain.getBlockByNumber(blockNumber - 1).map { parent =>
      val header = BlockHeader(
        parentHash = parent.header.hash,
        ommersHash = ByteString(kec256(encode(toRlpList(ommers)))),
        beneficiary = beneficiary.bytes,
        stateRoot = ByteString.empty,
        transactionsRoot = buildMpt(transactions, new RLPByteArraySerializable[SignedTransaction]),
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

      val body = BlockBody(transactions, ommers)
      val block = Block(header, body)

      ledger.prepareBlock(block, blockchainStorages, validators).right.map { case (BlockResult(_, gasUsed, receipts), stateRoot) =>
        val receiptsLogs: Seq[Array[Byte]] = BloomFilter.EmptyBloomFilter.toArray +: receipts.map(_.logsBloomFilter.toArray)
        val bloomFilter = ByteString(or(receiptsLogs: _*))

        block.copy(header = block.header.copy(
          stateRoot = stateRoot,
          receiptsRoot = buildMpt(receipts, new RLPByteArraySerializable[Receipt]),
          logsBloom = bloomFilter,
          gasUsed = gasUsed))
      }
    }.getOrElse(Left(NoParent))
  }

  //returns maximal limit to be able to include as many transactions as possible
  private def calculateGasLimit(parentGas: BigInt): BigInt = {
    val GasLimitBoundDivisor: Int = 1024

    val gasLimitDifference = parentGas / GasLimitBoundDivisor
    parentGas + gasLimitDifference - 1
  }

  private def buildMpt[K](entities: Seq[K], vSerializable: RLPByteArraySerializable[K]): ByteString = {
    val mpt = MerklePatriciaTrie[Int, K](
      source = new NodeStorage(EphemDataSource()),
      hashFn = (input: Array[Byte]) => kec256(input)
    )(intByteArraySerializable, vSerializable)
    val hash = entities.zipWithIndex.foldLeft(mpt) { case (trie, (value, key)) => trie.put(key, value) }.getRootHash
    ByteString(hash)
  }
}

object BlockGenerator {
  case object NoParent extends BlockPreparationError
}
