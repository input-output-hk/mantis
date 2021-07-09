package io.iohk.ethereum.consensus.mining

import org.bouncycastle.crypto.AsymmetricCipherKeyPair

/** Enumerates the known consensus protocols that Mantis can use.
  * For the respective implementations, see [[Mining Consensus]].
  */
sealed trait Protocol {

  /** We use this `name` to specify the protocol in configuration.
    *
    * @see [[mining.Protocol.Names]]
    */
  def name: String
}

object Protocol {
  object Names {
    // This is the standard Ethereum PoW consensus protocol.
    final val PoW = "pow"

    final val MockedPow = "mocked"

    final val RestrictedPoW = "restricted-pow"
  }

  sealed abstract class ProtocolImpl(val name: String) extends Protocol

  /** Mocked pow consensus algorithm used for tests etc. */
  case object MockedPow extends ProtocolImpl(Names.MockedPow)

  /** The standard Ethereum PoW consensus protocol. */
  case object PoW extends ProtocolImpl(Names.PoW)

  /** Non-standard ethereum PoW consensus protocol, which allows restricting list of possible miners.
    * Main differences from basic PoW consensus protocol:
    *   - Each miner, signs header data before mining i.e prepared header without mixHash and Nonce, and appends this
    *     signature to blockheader.extraData field. Only such prepared header is mined upon.
    *   - Each validator, checks (in addition to standard blockheader validations):
    *       a) if blockheader.extraData field has at most 97 bytes length (32 bytes of standard extraData + 65 bytes
    *          for ECDSA signature
    *       b) if signature is a valid signature over all blockheader data except: mixHash, Nonce, last 65 bytes of
    *          extraData field (those bytes are signature itself)
    *       c) if public key recovered from correct signature is contained within allowedMinersPublicKeys set defined
    *          for given chain
    */
  case object RestrictedPoW extends ProtocolImpl(Names.RestrictedPoW)

  /** All the known protocols. If a protocol is not put here, then it cannot be used to run Mantis. */
  final val KnownProtocols: Set[ProtocolImpl] = Set(
    PoW,
    MockedPow,
    RestrictedPoW
  )

  final val KnownProtocolNames: Set[String] = KnownProtocols.map(_.name)

  def find(name: String): Option[Protocol] = KnownProtocols.find(_.name == name)

  private[consensus] def apply(name: String): Protocol =
    find(name).getOrElse {
      throw new IllegalArgumentException("Unknown protocol " + name)
    }

  sealed abstract class AdditionalPoWProtocolData
  case object NoAdditionalPoWData extends AdditionalPoWProtocolData
  case class RestrictedPoWMinerData(miningNodeKey: AsymmetricCipherKeyPair) extends AdditionalPoWProtocolData
}
