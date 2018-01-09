package io.iohk.ethereum.consensus
package demo

import akka.actor.ActorRef
import io.iohk.ethereum.nodebuilder._

class DemoConsensusMinerBuilder(node: Node, demoConsensusConfig: DemoConsensusConfig) {
  import node._

  lazy val miner: ActorRef = actorSystem.actorOf(DemoConsensusMiner.props(
    blockchain,
    blockGenerator,
    ommersPool,
    pendingTransactionsManager,
    syncController,
    demoConsensusConfig,
    ethService,
    consensus
  ))
}
