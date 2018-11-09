package io.iohk.ethereum.network

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.{crypto, rlp}
import io.iohk.ethereum.rlp.RLPEncoder
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.util.BigIntegers
import scala.util.{Failure, Try}

package object discovery {

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
      val v = (signatureBytes.last + 27).toByte

      ECDSASignature(r, s, v)
    }
  }

  private[discovery] def encodePacket[M <: Message](msg: M, keyPair: AsymmetricCipherKeyPair)(implicit rlpEnc: RLPEncoder[M]): ByteString = {
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

  private[discovery] def extractMessage(packet: Packet): Try[Message] = Try {
    packet.packetType match {
      case Ping.packetType => rlp.decode[Ping](packet.data.toArray[Byte])
      case Pong.packetType => rlp.decode[Pong](packet.data.toArray[Byte])
      case FindNode.packetType => rlp.decode[FindNode](packet.data.toArray[Byte])
      case Neighbours.packetType => rlp.decode[Neighbours](packet.data.toArray[Byte])
      case _ => throw new RuntimeException(s"Unknown packet type ${packet.packetType}")
    }
  }

  private[discovery] def decodePacket(input: ByteString): Try[Packet] = {
    if (input.length < 98) {
      Failure(new RuntimeException("Bad message"))
    } else {
      val packet = Try(Packet(input))
      val mdcCheck = crypto.kec256(input.drop(32))

      if (packet.toOption.exists(_.mdc == mdcCheck)) packet
      else Failure(new RuntimeException("MDC check failed"))
    }
  }

}
