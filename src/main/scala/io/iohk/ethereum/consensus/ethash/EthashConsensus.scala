package io.iohk.ethereum.consensus
package ethash

import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidator, BlockHeaderValidatorImpl}

/**
 * Implements standard Ethereum consensus (ethash PoW).
 */
class EthashConsensus(
  blockchainConfig: BlockchainConfig,
  miningConfig: MiningConfig
) extends Consensus {
  private[this] val defaultValidator = new BlockHeaderValidatorImpl(blockchainConfig)
  private[this] val powValidator = new ethash.validators.BlockHeaderValidatorImpl(blockchainConfig)

  private[this] val ethashValidator = new BlockHeaderValidator {
    def validate(
      blockHeader: BlockHeader,
      getBlockHeaderByHash: ByteString ⇒ Option[BlockHeader]
    ): Either[BlockHeaderError, BlockHeaderValid] = {

      for {
        _ ← defaultValidator.validate(blockHeader, getBlockHeaderByHash)
        _ ← powValidator.validate(blockHeader, getBlockHeaderByHash)
      } yield BlockHeaderValid
    }
  }

  def blockHeaderValidator: BlockHeaderValidator = ethashValidator

  def startMiningProcess(node: Node): Unit = {
    val minerBuilder = new MinerBuilder(node, miningConfig)
    val miner = minerBuilder.miner
    miner ! Miner.StartMining
  }

  /**
   * Starts the consensus protocol.
   */
  def startProtocol(node: Node): Unit = {/* nothing special to do here, since it is all handled by the miner*/}
}
