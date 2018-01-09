package io.iohk.ethereum.consensus.ethash.blocks

import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks._
import io.iohk.ethereum.consensus.ethash.validators.EthashValidators
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{BlockPreparationError, BlockPreparator}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.BlockchainConfig

import scala.collection.immutable

/** Internal API, used for testing (especially mocks) */
trait EthashBlockGenerator extends BlockGenerator {
  type X = Ommers

  /** An empty `X` */
  def emptyX: Ommers
}

class EthashBlockGeneratorImpl(
  validators: EthashValidators,
  blockchain: Blockchain,
  blockchainConfig: BlockchainConfig,
  consensusConfig: ConsensusConfig,
  val blockPreparator: BlockPreparator,
  blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends BlockGeneratorImpl(
  blockchain,
  blockchainConfig,
  consensusConfig,
  blockPreparator,
  blockTimestampProvider
) with EthashBlockGenerator {

  protected def newBlockBody(transactions: immutable.Seq[SignedTransaction], ommers: Ommers): BlockBody = {
    BlockBody(transactions, ommers)
  }

  protected def prepareHeader(
    blockNumber: BigInt, parent: Block,
    beneficiary: Address, blockTimestamp: Long,
    ommers: Ommers
  ): BlockHeader =
    defaultPrepareHeader(blockNumber, parent, beneficiary, blockTimestamp, ommers)


  /** An empty `X` */
  def emptyX: Ommers = Nil

  def generateBlockForMining(
    parent: Block,
    transactions: Seq[SignedTransaction],
    beneficiary: Address,
    ommers: Ommers
  ): Either[BlockPreparationError, PendingBlock] = {
    val pHeader = parent.header
    val blockNumber = pHeader.number + 1
    val parentHash = pHeader.hash

    val ommersV = validators.ommersValidator

    val result: Either[InvalidOmmers, PendingBlockAndState] = ommersV
      .validate(parentHash, blockNumber, ommers, blockchain)
      .left.map(InvalidOmmers).flatMap { _ =>

        val prepared = prepareBlock(parent, transactions, beneficiary, blockNumber, blockPreparator, ommers)
        Right(prepared)
      }

    result.right.foreach(b => cache.updateAndGet((t: List[PendingBlockAndState]) => (b :: t).take(blockCacheSize)))

    result.map(_.pendingBlock)
  }

  def withBlockTimestampProvider(blockTimestampProvider: BlockTimestampProvider): EthashBlockGeneratorImpl =
    new EthashBlockGeneratorImpl(
      validators,
      blockchain,
      blockchainConfig,
      consensusConfig,
      blockPreparator,
      blockTimestampProvider
    )
}
