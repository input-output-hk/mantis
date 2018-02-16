package io.iohk.ethereum
package consensus
package ethash

import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.consensus.ethash.Miner.MinerMsg
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidator, BlockHeaderValidatorImpl}

/**
 * Implements standard Ethereum consensus (ethash PoW).
 */
class EthashConsensus(
  blockchainConfig: BlockchainConfig,
  val config: FullConsensusConfig[MiningConfig]
) extends Consensus {

  type Config = MiningConfig

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

  private[this] val atomicMiner = new AtomicReference[Option[ActorRef]](None)
  private[this] def sendMiner(msg: MinerMsg): Unit =
    atomicMiner.get().foreach(_ ! msg)

  private[this] def startMiningProcess(node: Node): Unit = {
    atomicMiner.get() match {
      case None ⇒
        val minerBuilder = new MinerBuilder(node, config.specific)
        val miner = minerBuilder.miner
        atomicMiner.set(Some(miner))

        sendMiner(Miner.StartMining)

      case _ ⇒
    }
  }

  private[this] def stopMiningProcess(): Unit = {
    sendMiner(Miner.StopMining)
  }

  def blockHeaderValidator: BlockHeaderValidator = ethashValidator

  /**
   * Starts the consensus protocol on the current `node`.
   */
  def startProtocol(node: Node): Unit = {
    if(config.miningEnabled) {
      startMiningProcess(node)
    }
  }

  def stopProtocol(): Unit = {
    if(config.miningEnabled) {
      stopMiningProcess()
    }
  }

  def protocol: Protocol = consensus.Ethash
}
