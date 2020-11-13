package io.iohk.ethereum.consensus.ethash.blocks

import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks.{
  BlockTimestampProvider,
  DefaultBlockTimestampProvider,
  PendingBlock,
  PendingBlockAndState
}
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.ethash.validators.ValidatorsExecutor
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.BlockHeader.getEncodedWithoutNonce
import io.iohk.ethereum.domain.{Address, Block, Blockchain, SignedTransaction}
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.utils.BlockchainConfig
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

class RestrictedEthashBlockGeneratorImpl(
    validators: ValidatorsExecutor,
    blockchain: Blockchain,
    blockchainConfig: BlockchainConfig,
    consensusConfig: ConsensusConfig,
    override val blockPreparator: BlockPreparator,
    difficultyCalc: DifficultyCalculator,
    minerKeyPair: AsymmetricCipherKeyPair,
    blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends EthashBlockGeneratorImpl(
      validators,
      blockchain,
      blockchainConfig,
      consensusConfig,
      blockPreparator,
      difficultyCalc,
      blockTimestampProvider
    ) {

  override def generateBlock(
      parent: Block,
      transactions: Seq[SignedTransaction],
      beneficiary: Address,
      x: Ommers
  ): PendingBlock = {
    val pHeader = parent.header
    val blockNumber = pHeader.number + 1
    val parentHash = pHeader.hash

    val ommers = validators.ommersValidator.validate(parentHash, blockNumber, x, blockchain) match {
      case Left(_) => emptyX
      case Right(_) => x
    }
    val prepared = prepareBlock(parent, transactions, beneficiary, blockNumber, blockPreparator, ommers)
    val preparedHeader = prepared.pendingBlock.block.header
    val encoded = getEncodedWithoutNonce(preparedHeader)
    val hash = crypto.kec256(encoded)
    val signed = ECDSASignature.sign(hash, minerKeyPair)
    val sigBytes = signed.toBytes
    val headerWithAdditionalExtraData = preparedHeader.copy(extraData = preparedHeader.extraData ++ sigBytes)
    val modifiedPrepared = prepared.copy(pendingBlock =
      prepared.pendingBlock.copy(block = prepared.pendingBlock.block.copy(header = headerWithAdditionalExtraData))
    )

    cache.updateAndGet { t: List[PendingBlockAndState] =>
      (modifiedPrepared :: t).take(blockCacheSize)
    }

    modifiedPrepared.pendingBlock
  }

}