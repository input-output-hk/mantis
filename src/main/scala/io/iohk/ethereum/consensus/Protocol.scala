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
}

object Protocol {
  object Names {
    // This is the standard Ethereum PoW consensus protocol.
    final val Ethash = "ethash"

    // Using the Raft implementation from atomix.io
    final val AtomixRaft = "atomix-raft"
  }

  sealed abstract class ProtocolImpl(val name: String) extends Protocol

  /** The standard Ethereum PoW consensus protocol. */
  case object Ethash extends ProtocolImpl(Names.Ethash)

  /** Raft consensus protocol. */
  case object AtomixRaft extends ProtocolImpl(Names.AtomixRaft)

  /** All the known protocols. If a protocol is not put here, then it cannot be used to run Mantis. */
  final val KnownProtocols = Set(
    Ethash,
    AtomixRaft
  )

  final val KnownProtocolNames = KnownProtocols.map(_.name)

  def find(name: String): Option[Protocol] = KnownProtocols.find(_.name == name)

  private[consensus] def apply(name: String): Protocol =
    find(name).getOrElse {
      throw new IllegalArgumentException("Unknown protocol " + name)
    }
}
