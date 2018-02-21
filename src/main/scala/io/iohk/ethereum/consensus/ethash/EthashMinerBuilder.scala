package io.iohk.ethereum.consensus.ethash

import akka.actor.ActorRef
import io.iohk.ethereum.consensus.FullConsensusConfig
import io.iohk.ethereum.nodebuilder._

class EthashMinerBuilder(
  node: Node,
  miningConfig: EthashConfig
) {
  import node._

  private[this] val config = FullConsensusConfig(consensusConfig, miningConfig)

  lazy val miner: ActorRef = actorSystem.actorOf(EthashMiner.props(
    blockchain,
    blockGenerator,
    ommersPool,
    pendingTransactionsManager,
    syncController,
    config,
    ethService
  ))
}
