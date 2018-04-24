package io.iohk.ethereum

import io.iohk.ethereum.nodebuilder.{StdNode, TestNode}
import io.iohk.ethereum.utils.Config

object Mantis {
  def main(args: Array[String]): Unit = {
    val node =
      if (Config.testmode) new TestNode
      else new StdNode

    node.start()
  }
}
