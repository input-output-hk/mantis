package io.iohk.ethereum.network.discovery.codecs

import java.net.InetAddress

import scala.util.Try

import io.iohk.scalanet.discovery.crypto.PublicKey
import io.iohk.scalanet.discovery.crypto.Signature
import io.iohk.scalanet.discovery.ethereum.EthereumNodeRecord
import io.iohk.scalanet.discovery.ethereum.Node
import io.iohk.scalanet.discovery.ethereum.v4.Payload
import io.iohk.scalanet.discovery.hash.Hash
import scodec.Attempt
import scodec.Codec
import scodec.DecodeResult
import scodec.Err
import scodec.bits.BitVector
import scodec.bits.ByteVector

import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPCodec
import io.iohk.ethereum.rlp.RLPCodec.Ops
import io.iohk.ethereum.rlp.RLPEncodeable
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.rlp.RLPImplicitConversions.toEncodeable
import io.iohk.ethereum.rlp.RLPImplicitDerivations._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList

/** RLP codecs based on https://github.com/ethereum/devp2p/blob/master/discv4.md */
object RLPCodecs extends ContentCodecs with PayloadCodecs {
  implicit def codecFromRLPCodec[T: RLPCodec]: Codec[T] =
    Codec[T](
      (value: T) => {
        val bytes = rlp.encode(value)
        Attempt.successful(BitVector(bytes))
      },
      (bits: BitVector) => {
        val tryDecode = Try(rlp.decode[T](bits.toByteArray))
        Attempt.fromTry(tryDecode.map(DecodeResult(_, BitVector.empty)))
      }
    )
}

trait ContentCodecs {
  implicit val inetAddressRLPCodec: RLPCodec[InetAddress] =
    implicitly[RLPCodec[Array[Byte]]].xmap(InetAddress.getByAddress(_), _.getAddress)

  implicit val bitVectorRLPCodec: RLPCodec[BitVector] =
    implicitly[RLPCodec[Array[Byte]]].xmap(BitVector(_), _.toByteArray)

  implicit val byteVectorRLPCodec: RLPCodec[ByteVector] =
    implicitly[RLPCodec[Array[Byte]]].xmap(ByteVector(_), _.toArray)

  implicit val hashRLPCodec: RLPCodec[Hash] =
    implicitly[RLPCodec[BitVector]].xmap(Hash(_), identity)

  implicit val publicKeyRLPCodec: RLPCodec[PublicKey] =
    implicitly[RLPCodec[BitVector]].xmap(PublicKey(_), identity)

  implicit val signatureRLPCodec: RLPCodec[Signature] =
    implicitly[RLPCodec[BitVector]].xmap(Signature(_), identity)

  implicit val nodeAddressRLPCodec: RLPCodec[Node.Address] =
    deriveLabelledGenericRLPCodec

  implicit val nodeRLPCodec: RLPCodec[Node] =
    RLPCodec.instance[Node](
      { case Node(id, address) =>
        RLPEncoder.encode(address).asInstanceOf[RLPList] :+ id
      },
      {
        case RLPList(items @ _*) if items.length == 4 =>
          val address = RLPList(items.take(3): _*).decodeAs[Node.Address]("address")
          val id = items(3).decodeAs[PublicKey]("id")
          Node(id, address)
      }
    )

  // https://github.com/ethereum/devp2p/blob/master/enr.md#rlp-encoding
  // content = [seq, k, v, ...]
  implicit val enrContentRLPCodec: RLPCodec[EthereumNodeRecord.Content] = {
    // Differentiating by predefined keys is a workaround for the situation that
    // EthereumNodeRecord holds ByteVectors, not RLPEncodeable instances in its map,
    // but as per the spec the content can be anything (up to a total of 300 bytes).
    // We need to be able to preserve the fidelity of the encoding over a roundtrip
    // so that we can verify signatures, so we have to be able to put things in the
    // map as bytes and later be able to tell whether they were originally an
    // RLPValue on an RLPList.
    // For now treat all predefined keys as bytes and everything else as RLP.
    import EthereumNodeRecord.Keys.Predefined

    RLPCodec.instance(
      { case EthereumNodeRecord.Content(seq, attrs) =>
        val kvs = attrs
          .foldRight(RLPList()) { case ((key, value), kvs) =>
            val k: RLPEncodeable = key
            val v: RLPEncodeable = if (Predefined(key)) value else rlp.rawDecode(value.toArray)
            k +: v +: kvs
          }
        seq +: kvs
      },
      { case RLPList(seq, kvs @ _*) =>
        val attrs = kvs
          .grouped(2)
          .collect { case Seq(k, v) =>
            val key = k.decodeAs[ByteVector]("key")
            val keyString = Try(new String(key.toArray)).getOrElse(key.toString)
            val value =
              if (Predefined(key)) {
                v.decodeAs[ByteVector](s"value of key '${keyString}'")
              } else {
                ByteVector(rlp.encode(v))
              }
            key -> value
          }
          .toSeq

        EthereumNodeRecord.Content(
          seq.decodeAs[Long]("seq"),
          attrs: _*
        )
      }
    )
  }

  // record = [signature, seq, k, v, ...]
  implicit val enrRLPCodec: RLPCodec[EthereumNodeRecord] =
    RLPCodec.instance(
      { case EthereumNodeRecord(signature, content) =>
        val contentList = RLPEncoder.encode(content).asInstanceOf[RLPList]
        signature +: contentList
      },
      { case RLPList(signature, content @ _*) =>
        EthereumNodeRecord(
          signature.decodeAs[Signature]("signature"),
          RLPList(content: _*).decodeAs[EthereumNodeRecord.Content]("content")
        )
      }
    )
}

trait PayloadCodecs { self: ContentCodecs =>

  implicit val payloadDerivationPolicy: DerivationPolicy =
    DerivationPolicy.default.copy(omitTrailingOptionals = true)

  implicit val pingRLPCodec: RLPCodec[Payload.Ping] =
    deriveLabelledGenericRLPCodec

  implicit val pongRLPCodec: RLPCodec[Payload.Pong] =
    deriveLabelledGenericRLPCodec

  implicit val findNodeRLPCodec: RLPCodec[Payload.FindNode] =
    deriveLabelledGenericRLPCodec

  implicit val neighborsRLPCodec: RLPCodec[Payload.Neighbors] =
    deriveLabelledGenericRLPCodec

  implicit val enrRequestRLPCodec: RLPCodec[Payload.ENRRequest] =
    deriveLabelledGenericRLPCodec

  implicit val enrResponseRLPCodec: RLPCodec[Payload.ENRResponse] =
    deriveLabelledGenericRLPCodec

  private object PacketType {
    val Ping: Byte = 0x01
    val Pong: Byte = 0x02
    val FindNode: Byte = 0x03
    val Neighbors: Byte = 0x04
    val ENRRequest: Byte = 0x05
    val ENRResponse: Byte = 0x06
  }

  implicit def payloadCodec: Codec[Payload] =
    Codec[Payload](
      (payload: Payload) => {
        val (packetType, packetData) =
          payload match {
            case x: Payload.Ping        => PacketType.Ping -> rlp.encode(x)
            case x: Payload.Pong        => PacketType.Pong -> rlp.encode(x)
            case x: Payload.FindNode    => PacketType.FindNode -> rlp.encode(x)
            case x: Payload.Neighbors   => PacketType.Neighbors -> rlp.encode(x)
            case x: Payload.ENRRequest  => PacketType.ENRRequest -> rlp.encode(x)
            case x: Payload.ENRResponse => PacketType.ENRResponse -> rlp.encode(x)
          }

        Attempt.successful(BitVector(packetType.toByte +: packetData))
      },
      (bits: BitVector) =>
        bits.consumeThen(8)(
          err => Attempt.failure(Err(err)),
          (head, tail) => {
            val packetType: Byte = head.toByte()
            val packetData: Array[Byte] = tail.toByteArray

            val tryPayload: Try[Payload] = Try {
              packetType match {
                case PacketType.Ping        => rlp.decode[Payload.Ping](packetData)
                case PacketType.Pong        => rlp.decode[Payload.Pong](packetData)
                case PacketType.FindNode    => rlp.decode[Payload.FindNode](packetData)
                case PacketType.Neighbors   => rlp.decode[Payload.Neighbors](packetData)
                case PacketType.ENRRequest  => rlp.decode[Payload.ENRRequest](packetData)
                case PacketType.ENRResponse => rlp.decode[Payload.ENRResponse](packetData)
                case other                  => throw new RuntimeException(s"Unknown packet type: ${other}")
              }
            }

            Attempt.fromTry(tryPayload.map(DecodeResult(_, BitVector.empty)))
          }
        )
    )
}
