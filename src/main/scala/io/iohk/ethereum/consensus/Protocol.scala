package io.iohk.ethereum.consensus

/**
 * Enumerates the known consensus protocols that Mantis can use.
 */
sealed trait Protocol {
  /**
   * We use this `name` to specify the protocol in configuration.
   *
   * @see [[io.iohk.ethereum.consensus.Protocol.Names]]
   */
  def name: String
}

object Protocol {
  object Names {
    // This is the standard Ethereum PoW consensus protocol.
    final val Ethash = "ethash"

    // This is a dummy consensus protocol for demonstration purposes (pluggable consensus)
    final val DemoConsensus = "demo-consensus"
  }

  def find(name: String): Option[Protocol] =
    name match {
      case Names.Ethash ⇒ Some(Ethash)
      case Names.DemoConsensus ⇒ Some(DemoPoS)
      case _ ⇒ None
    }

  private[consensus] def apply(name: String): Protocol =
    find(name) match {
      case Some(protocol) ⇒ protocol
      case None ⇒
        require(requirement = false, "Protocol " + name + " is enumerated")
        throw new Exception()
    }
}

case object Ethash extends Protocol {
  final val name = Protocol.Names.Ethash
}

case object DemoPoS extends Protocol {
  final val name = Protocol.Names.DemoConsensus
}
