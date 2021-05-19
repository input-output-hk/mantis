package io.iohk.ethereum.network.p2p.messages

import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPException, RLPList, RLPSerializable, rawDecode}

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
      case _ => throw new RLPException("Cannot decode Capability")
    }
  }

  object Capabilities {
    val Eth63Capability: Capability = Capability("eth", ProtocolVersions.PV63.toByte)
    //FIXME This PV is considered WIP and needs to be reassessed
    val Etc164Capability: Capability = Capability("etc", ProtocolVersions.PV164.toByte)

    val All: Seq[Capability] = Seq(Etc164Capability, Eth63Capability)
  }
}
