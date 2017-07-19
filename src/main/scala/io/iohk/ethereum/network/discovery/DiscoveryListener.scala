package io.iohk.ethereum.network.discovery

import java.net.{InetSocketAddress, URI}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.agent.Agent
import akka.io.{IO, Udp}
import akka.util.ByteString
import io.iohk.ethereum.{crypto, rlp}
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.rlp.RLPEncoder

import scala.util.{Failure, Success, Try}
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.util.BigIntegers

class DiscoveryListener(
    discoveryConfig: DiscoveryConfig,
    nodeStatusHolder: Agent[NodeStatus])
  extends Actor with ActorLogging {

  import context.system
  import DiscoveryListener._

  var subscribers: Set[ActorRef] = Set.empty

  override def receive: Receive = handleSubscribe orElse {
    case Start =>
      IO(Udp) ! Udp.Bind(self, new InetSocketAddress(discoveryConfig.interface, discoveryConfig.port))

    case Udp.Bound(local) =>
      nodeStatusHolder.send(_.copy(discoveryStatus = ServerStatus.Listening(local)))
      context.become(ready(sender()))
  }

  def handleSubscribe: Receive = {
    case Subscribe =>
      subscribers += sender()
  }

  def ready(socket: ActorRef): Receive = handleSubscribe orElse {
    case Udp.Received(data, remote) =>
      val msgReceivedTry = for {
        packet <- decodePacket(data)
        message <- extractMessage(packet)
      } yield MessageReceived(message, remote, packet)

      msgReceivedTry match {
        case Success(msgReceived) => subscribers.foreach(_ ! msgReceived)
        case Failure(ex) => log.error(ex, "Unable to decode discovery packet from {}", remote)
      }

    case sm @ SendMessage(message, to) =>
      val packet = encodePacket(message, nodeStatusHolder().key)(sm.rlpEnc)
      socket ! Udp.Send(packet, to)
  }

  private def decodePacket(input: ByteString): Try[Packet] = {
    if (input.length < 98) {
      Failure(new RuntimeException("Bad message"))
    } else {
      val packet = Try(Packet(input))
      val mdcCheck = crypto.kec256(input.drop(32))

      if (packet.toOption.exists(_.mdc == mdcCheck)) packet
      else Failure(new RuntimeException("MDC check failed"))
    }
  }

  def extractMessage(packet: Packet): Try[Message] = Try {
    packet.packetType match {
      case Ping.packetType => rlp.decode[Ping](packet.data.toArray[Byte])
      case Pong.packetType => rlp.decode[Pong](packet.data.toArray[Byte])
      case FindNode.packetType => rlp.decode[FindNode](packet.data.toArray[Byte])
      case Neighbours.packetType => rlp.decode[Neighbours](packet.data.toArray[Byte])
      case _ => throw new RuntimeException(s"Unknown packet type ${packet.packetType}")
    }
  }

  def encodePacket[M <: Message](msg: M, keyPair: AsymmetricCipherKeyPair)(implicit rlpEnc: RLPEncoder[M]): ByteString = {
    val encodedData = rlp.encode(msg)

    val payload = Array(msg.packetType) ++ encodedData
    val forSig = crypto.kec256(payload)
    val signature = ECDSASignature.sign(forSig, keyPair, None)

    val sigBytes =
      BigIntegers.asUnsignedByteArray(32, signature.r.bigInteger) ++
      BigIntegers.asUnsignedByteArray(32, signature.s.bigInteger) ++
      Array[Byte]((signature.v - 27).toByte)

    val forSha = sigBytes ++ Array(msg.packetType) ++ encodedData
    val mdc = crypto.kec256(forSha)

    ByteString(mdc ++ sigBytes ++ Array(msg.packetType) ++ encodedData)
  }

}

object DiscoveryListener {
  def props(config: DiscoveryConfig, nodeStatusHolder: Agent[NodeStatus]): Props =
    Props(new DiscoveryListener(config, nodeStatusHolder))

  case object Start
  case object Subscribe

  case class SendMessage[M <: Message](message: M, to: InetSocketAddress)(implicit val rlpEnc: RLPEncoder[M])
  case class MessageReceived(message: Message, from: InetSocketAddress, packet: Packet)

  object Packet {
    private val MdcLength = 32
    private val PacketTypeByteIndex = MdcLength + ECDSASignature.EncodedLength
    private val DataOffset = PacketTypeByteIndex + 1
  }

  case class Packet(wire: ByteString) {
    import Packet._

    val nodeId: ByteString = {
      val msgHash = crypto.kec256(wire.drop(MdcLength + ECDSASignature.EncodedLength))
      signature.publicKey(msgHash.toArray[Byte], None).map(ByteString.apply)
    }.get

    def data: ByteString = wire.drop(DataOffset)

    def packetType: Byte = wire(PacketTypeByteIndex)

    def mdc: ByteString = wire.take(MdcLength)

    def signature: ECDSASignature = {
      val signatureBytes = wire.drop(MdcLength).take(ECDSASignature.EncodedLength)
      val r = signatureBytes.take(ECDSASignature.RLength)
      val s = signatureBytes.drop(ECDSASignature.RLength).take(ECDSASignature.SLength)
      val v = ByteString(Array[Byte]((signatureBytes(ECDSASignature.EncodedLength - 1) + 27).toByte))

      ECDSASignature(r, s, v)
    }
  }

}
