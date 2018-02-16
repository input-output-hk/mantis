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
  final def isEthash: Boolean =
    this match {
      case Ethash ⇒ true
      case _ ⇒ false
    }
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

  def find(name: String): Option[Protocol] =
    name match {
      case Names.Ethash ⇒ Some(Ethash)
      case Names.Demo0 ⇒ Some(Demo0)
      case Names.AtomixRaft ⇒ Some(AtomixRaft)
      case _ ⇒ None
    }

  private[consensus] def apply(name: String): Protocol =
    find(name) match {
      case Some(protocol) ⇒ protocol
      case None ⇒
        require(requirement = false, "Unknown protocol " + name)
        throw new Exception()
    }
}

sealed abstract class ProtocolImpl private[consensus](val name: String) extends Protocol

/** The standard Ethereum PoW consensus protocol. */
case object Ethash extends ProtocolImpl(Protocol.Names.Ethash)

/** A dump protocol used internally for demonstration purposes */
case object Demo0 extends ProtocolImpl(Protocol.Names.Demo0)

/** Raft consensus protocol */
case object AtomixRaft extends ProtocolImpl(Protocol.Names.AtomixRaft)
