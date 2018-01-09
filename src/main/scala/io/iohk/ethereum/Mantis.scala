package io.iohk.ethereum

import io.iohk.ethereum.nodebuilder.StdNode

object Mantis {
  def main(args: Array[String]): Unit = {
    val node = new StdNode
    node.start()
  }
}
