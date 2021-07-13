package io.iohk.ethereum.network.p2p.messages

import io.iohk.ethereum.rlp.RLPEncodeable
import io.iohk.ethereum.rlp.RLPException
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.RLPSerializable
import io.iohk.ethereum.rlp.RLPValue
import io.iohk.ethereum.rlp.rawDecode

sealed trait ProtocolFamily
object ProtocolFamily {
  final case object ETH extends ProtocolFamily
  final case object ETC extends ProtocolFamily
  implicit class ProtocolFamilyEnc(val msg: ProtocolFamily) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = msg match {
      case ETH => RLPValue("eth".getBytes())
      case ETC => RLPValue("etc".getBytes())
    }
  }
}

sealed abstract class Capability(val name: ProtocolFamily, val version: Byte)

object Capability {
  case object ETH63 extends Capability(ProtocolFamily.ETH, 63) //scalastyle:ignore magic.number
  case object ETH64 extends Capability(ProtocolFamily.ETH, 64) //scalastyle:ignore magic.number
  case object ETC64 extends Capability(ProtocolFamily.ETC, 64) //scalastyle:ignore magic.number

  def parse(s: String): Option[Capability] = s match {
    case "eth/63" => Some(ETH63)
    case "eth/64" => Some(ETH64)
    case "etc/64" => Some(ETC64)
    case _        => None // TODO: log unknown capability?
  }

  def parseUnsafe(s: String): Capability =
    parse(s).getOrElse(throw new RuntimeException(s"Capability $s not supported by Mantis"))

  def negotiate(c1: List[Capability], c2: List[Capability]): Option[Capability] =
    c1.intersect(c2) match {
      case Nil => None
      case l   => Some(best(l))
    }

  //TODO consider how this scoring should be handled with 'snap' and other extended protocols
  def best(capabilities: List[Capability]): Capability =
    capabilities.maxBy(_.version)

  implicit class CapabilityEnc(val msg: Capability) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = RLPList(msg.name.toRLPEncodable, msg.version)
  }

  implicit class CapabilityDec(val bytes: Array[Byte]) extends AnyVal {
    def toCapability: Option[Capability] = CapabilityRLPEncodableDec(rawDecode(bytes)).toCapability
  }

  implicit class CapabilityRLPEncodableDec(val rLPEncodeable: RLPEncodeable) extends AnyVal {
    def toCapability: Option[Capability] = rLPEncodeable match {
      case RLPList(name, version) => parse(s"${stringEncDec.decode(name)}/${byteEncDec.decode(version)}")
      case _                      => throw new RLPException("Cannot decode Capability")
    }
  }

}
