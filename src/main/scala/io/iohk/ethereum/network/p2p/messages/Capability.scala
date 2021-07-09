package io.iohk.ethereum.network.p2p.messages

import io.iohk.ethereum.rlp.RLPEncodeable
import io.iohk.ethereum.rlp.RLPException
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.RLPSerializable
import io.iohk.ethereum.rlp.rawDecode

case class Capability(name: String, version: Byte)

object Capability {
  def negotiate(c1: List[Capability], c2: List[Capability]): Option[Capability] =
    c1.intersect(c2) match {
      case Nil => None
      case l   => Some(best(l))
    }

  private val pattern = "(.*)/(\\d*)".r

  def parseUnsafe(protocolVersion: String): Capability =
    protocolVersion match {
      case pattern(name, version) =>
        val c = Capability(name, version.toByte)
        if (Capabilities.All.contains(c))
          c
        else
          throw new RuntimeException(s"Capability $protocolVersion not supported by Mantis")
      case _ => throw new RuntimeException(s"Unable to parse capability $protocolVersion")
    }

  //TODO consider how this scoring should be handled with 'snap' and other extended protocols
  def best(capabilities: List[Capability]): Capability =
    capabilities.maxBy(_.version)

  implicit class CapabilityEnc(val msg: Capability) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = RLPList(msg.name, msg.version)
  }

  implicit class CapabilityDec(val bytes: Array[Byte]) extends AnyVal {
    def toCapability: Capability = CapabilityRLPEncodableDec(rawDecode(bytes)).toCapability
  }

  implicit class CapabilityRLPEncodableDec(val rLPEncodeable: RLPEncodeable) extends AnyVal {
    def toCapability: Capability = rLPEncodeable match {
      case RLPList(name, version) => Capability(name, version)
      case _                      => throw new RLPException("Cannot decode Capability")
    }
  }

  object Capabilities {
    val Eth63Capability: Capability = ProtocolVersions.ETH63
    val Eth64Capability: Capability = ProtocolVersions.ETH64
    val Etc64Capability: Capability = ProtocolVersions.ETC64

    val All: Seq[Capability] = Seq(ProtocolVersions.ETC64, ProtocolVersions.ETH63, ProtocolVersions.ETH64)
  }
}
