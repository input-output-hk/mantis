package io.iohk.ethereum
package consensus
package ethash

import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import io.iohk.ethereum.consensus.ethash.Miner.MinerMsg
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.validators.BlockHeaderValidator

/**
 * Implements standard Ethereum consensus (ethash PoW).
 */
class EthashConsensus(
  blockchainConfig: BlockchainConfig,
  val config: FullConsensusConfig[MiningConfig]
) extends Consensus {

  type Config = MiningConfig

  private[this] val ethashValidator = new ethash.validators.EthashBlockHeaderValidator(blockchainConfig)

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
