package io.iohk.ethereum.evm.util

import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.Logger

object DumpChainApp {
  def main(args: Array[String]): Unit = {

    new Node with Logger {
      override def shutdown(): Unit = ()
      actorSystem.actorOf(DumpChainActor.props(peerManager), "dumper")
    }

  }
}
