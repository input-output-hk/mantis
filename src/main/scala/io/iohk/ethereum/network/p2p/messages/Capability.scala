package io.iohk.ethereum.network.p2p.messages

import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPException, RLPList, RLPSerializable, rawDecode}

case class Capability(name: String, version: Byte)

object Capability {
  def negotiate(c1: List[Capability], c2: List[Capability]): Option[Capability] =
    c1.intersect(c2).maxByOption(_.version) // FIXME ignores branches and other protocols

  private val pattern = "(.*)/(\\d*)".r

  def from(protocolVersion: String): Capability =
    protocolVersion match {
      case pattern(name, version) => Capability(name, version.toByte)
      case _ => throw new RuntimeException(s"Unable to parse capability $protocolVersion")
    }

  //TODO consider how this scoring should be handled with snap and other extended protocols
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
      case _ => throw new RLPException("Cannot decode Capability")
    }
  }

  object Capabilities {
    val Eth63Capability: Capability = ProtocolVersions.ETH63
    val Etc64Capability: Capability = ProtocolVersions.ETC64

    val All: Seq[Capability] = Seq(ProtocolVersions.ETC64, ProtocolVersions.ETH63)
  }
}
