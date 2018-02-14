package io.iohk.ethereum.consensus.ethash

import akka.actor.ActorRef
import io.iohk.ethereum.nodebuilder._

class MinerBuilder(
  node: Node,
  miningConfig: MiningConfig
) {
  import node._

  lazy val miner: ActorRef = actorSystem.actorOf(Miner.props(
    blockchain,
    blockGenerator,
    ommersPool,
    pendingTransactionsManager,
    syncController,
    miningConfig,
    ethService,
    consensus
  ))
}
