package io.iohk.ethereum

import io.iohk.ethereum.nodebuilder.{StdNode, TestNode}
import io.iohk.ethereum.utils.{Config, Logger}

object Mantis extends Logger {
  def main(args: Array[String]): Unit = {
    val node =
      if (Config.testmode) {
        log.info("Starting Mantis in test mode")
        new TestNode
      } else new StdNode

    log.info("Using network {}", Config.blockchains.network)

    node.start()
  }
}
