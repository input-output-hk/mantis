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

    final val MockedPow = "mocked"

    final val RestrictedEthash = "restricted-ethash"
  }

  sealed abstract class ProtocolImpl(val name: String) extends Protocol

  /** Mocked pow consensus algorithm used for tests etc. */
  case object MockedPow extends ProtocolImpl(Names.MockedPow)

  /** The standard Ethereum PoW consensus protocol. */
  case object Ethash extends ProtocolImpl(Names.Ethash)

  /** The standard Ethereum PoW consensus protocol. */
  case object RestrictedEthash extends ProtocolImpl(Names.RestrictedEthash)

  /** All the known protocols. If a protocol is not put here, then it cannot be used to run Mantis. */
  final val KnownProtocols = Set(
    Ethash,
    MockedPow,
    RestrictedEthash
  )

  final val KnownProtocolNames = KnownProtocols.map(_.name)

  def find(name: String): Option[Protocol] = KnownProtocols.find(_.name == name)

  private[consensus] def apply(name: String): Protocol =
    find(name).getOrElse {
      throw new IllegalArgumentException("Unknown protocol " + name)
    }
}
