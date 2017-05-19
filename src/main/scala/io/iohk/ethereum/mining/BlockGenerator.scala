package io.iohk.ethereum.mining

import java.time.Instant

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.domain.{Block, BlockHeader, Receipt, SignedTransaction, _}
import io.iohk.ethereum.ledger.Ledger.{BlockPreparationResult, BlockResult}
import io.iohk.ethereum.ledger.{BlockPreparationError, BloomFilter, Ledger}
import io.iohk.ethereum.mining.BlockGenerator.{InvalidOmmers, NoParent}
import io.iohk.ethereum.mpt.{ByteArraySerializable, MerklePatriciaTrie}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteUtils.or
import io.iohk.ethereum.validators.MptListValidator.intByteArraySerializable
import io.iohk.ethereum.validators.OmmersValidator.OmmersError
import io.iohk.ethereum.validators.Validators
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}
import io.iohk.ethereum.crypto._

class BlockGenerator(blockchainStorages: BlockchainStorages, blockchainConfig: BlockchainConfig, ledger: Ledger, validators: Validators) {
  val difficulty = new DifficultyCalculator(blockchainConfig)

  //todo add logic
  private val fakeAddress = 42
  private lazy val block = generateBlockForMining(1, Nil, Nil, Address(fakeAddress)).fold(e => null, identity)

  def generateBlockForMining(): Block = block

  def generateBlockForMining(blockNumber: BigInt, transactions: Seq[SignedTransaction], ommers: Seq[BlockHeader], beneficiary: Address):
  Either[BlockPreparationError, Block] = {
    val blockchain = BlockchainImpl(blockchainStorages)

    validators.ommersValidator.validate(blockNumber, ommers, blockchain).left.map(InvalidOmmers).flatMap { _ =>
      val blockTimestamp = Instant.now.getEpochSecond
      blockchain.getBlockByNumber(blockNumber - 1).map { parent =>
        val header = BlockHeader(
          parentHash = parent.header.hash,
          ommersHash = ByteString(kec256(ommers.toBytes: Array[Byte])),
          beneficiary = beneficiary.bytes,
          stateRoot = ByteString.empty,
          transactionsRoot = buildMpt(transactions, SignedTransaction.byteArraySerializable),
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

        ledger.prepareBlock(block, blockchainStorages, validators).right.map { case BlockPreparationResult(BlockResult(_, gasUsed, receipts), stateRoot) =>
          val receiptsLogs: Seq[Array[Byte]] = BloomFilter.EmptyBloomFilter.toArray +: receipts.map(_.logsBloomFilter.toArray)
          val bloomFilter = ByteString(or(receiptsLogs: _*))

          block.copy(header = block.header.copy(
            stateRoot = stateRoot,
            receiptsRoot = buildMpt(receipts, Receipt.byteArraySerializable),
            logsBloom = bloomFilter,
            gasUsed = gasUsed))
        }
      }.getOrElse(Left(NoParent))
    }
  }

  //returns maximal limit to be able to include as many transactions as possible
  private def calculateGasLimit(parentGas: BigInt): BigInt = {
    val GasLimitBoundDivisor: Int = 1024

    val gasLimitDifference = parentGas / GasLimitBoundDivisor
    parentGas + gasLimitDifference - 1
  }

  private def buildMpt[K](entities: Seq[K], vSerializable: ByteArraySerializable[K]): ByteString = {
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
  case class InvalidOmmers(reason: OmmersError) extends BlockPreparationError
}
