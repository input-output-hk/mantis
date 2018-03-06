package io.iohk.ethereum.consensus

/**
 * Enumerates the known consensus protocols that Mantis can use.
 * For the respective implementations, see [[io.iohk.ethereum.consensus.Consensus Consensus]].
 */
sealed trait Protocol {
  /**
   * We use this `name` to specify the protocol in configuration.
   *
   * @see [[io.iohk.ethereum.consensus.Protocol.Names]]
   */
  def name: String

  /** Returns `true` if this is the standard Ethereum PoW consensus protocol (`ethash`). */
  def isEthash: Boolean
}

object Protocol {
  object Names {
    // This is the standard Ethereum PoW consensus protocol.
    final val Ethash = "ethash"

    // This is a dummy consensus protocol for demonstration purposes (pluggable consensus)
    final val Demo0 = "demo-consensus"

    // Using the Raft implementation from atomix.io
    final val AtomixRaft = "atomix-raft"
  }

  sealed abstract class ProtocolImpl(val name: String, val isEthash: Boolean) extends Protocol

  /** The standard Ethereum PoW consensus protocol. */
  case object Ethash extends ProtocolImpl(Names.Ethash, true)


  /** Raft consensus protocol. */
  case object AtomixRaft extends ProtocolImpl(Names.AtomixRaft, false)

  /** All the known protocols. If a protocol is not put here, then it cannot be used to run Mantis. */
  final val KnownProtocols = Set(
    Ethash,
    AtomixRaft
  )

  final val KnownProtocolNames = KnownProtocols.map(_.name)

  def find(name: String): Option[Protocol] = KnownProtocols.find(_.name == name)

  private[consensus] def apply(name: String): Protocol =
    find(name).getOrElse {
      require(requirement = false, "Unknown protocol " + name)
      throw new Exception()
    }
}
