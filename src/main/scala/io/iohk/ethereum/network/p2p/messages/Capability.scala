package io.iohk.ethereum.network.p2p.messages

import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPSerializable, rawDecode}

case class Capability(name: String, version: Byte)

object Capability {
  implicit class CapabilityEnc(val msg: Capability) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = RLPList(msg.name, msg.version)
  }

  implicit class CapabilityDec(val bytes: Array[Byte]) extends AnyVal {
    def toCapability: Capability = CapabilityRLPEncodableDec(rawDecode(bytes)).toCapability
  }

  implicit class CapabilityRLPEncodableDec(val rLPEncodeable: RLPEncodeable) extends AnyVal {
    def toCapability: Capability = rLPEncodeable match {
      case RLPList(name, version) => Capability(name, version)
      case _ => throw new RuntimeException("Cannot decode Capability")
    }
  }

  object Capabilities {
    val Eth63Capability: Capability = Capability("eth", Versions.PV63.toByte)
    val Eth64Capability: Capability = Capability("eth", Versions.PV64.toByte)

    val All: Seq[Capability] = Seq(Eth64Capability, Eth63Capability)
  }
}
