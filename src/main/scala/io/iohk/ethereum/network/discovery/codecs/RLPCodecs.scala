package io.iohk.ethereum.network.discovery.codecs

import io.iohk.scalanet.discovery.crypto.{PublicKey, Signature}
import io.iohk.scalanet.discovery.ethereum.{Node, EthereumNodeRecord}
import io.iohk.scalanet.discovery.ethereum.v4.Payload
import io.iohk.scalanet.discovery.hash.Hash
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.{RLPCodec, RLPList}
import io.iohk.ethereum.rlp.RLPCodec.Ops
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicitDerivations._
import scodec.{Codec, Attempt, Err, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import java.net.InetAddress
import scala.collection.SortedMap
import scala.util.Try
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.rlp.RLPDecoder

/** RLP codecs based on https://github.com/ethereum/devp2p/blob/master/discv4.md */
object RLPCodecs {

  implicit val policy: DerivationPolicy = DerivationPolicy(omitTrailingOptionals = true)

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
        RLPEncoder.encode(address).asInstanceOf[RLPList] ++ RLPList(id)
      },
      {
        case list @ RLPList(items @ _*) if items.length >= 4 =>
          val address = RLPDecoder.decode[Node.Address](list)
          val id = RLPDecoder.decode[PublicKey](items(3))
          Node(id, address)
      }
    )

  // https://github.com/ethereum/devp2p/blob/master/enr.md#rlp-encoding
  // `record = [signature, seq, k, v, ...]`
  implicit val enrRLPCodec: RLPCodec[EthereumNodeRecord] =
    RLPCodec.instance(
      { case EthereumNodeRecord(signature, EthereumNodeRecord.Content(seq, attrs)) =>
        val kvs = attrs
          .foldRight(RLPList()) { case ((k, v), kvs) =>
            k.toArray :: v.toArray :: kvs
          }

        signature.toByteArray :: seq :: kvs
      },
      { case RLPList(signature, seq, kvs @ _*) =>
        val attrs = kvs
          .grouped(2)
          .collect { case Seq(k, v) =>
            rlp.decode[ByteVector](k) -> rlp.decode[ByteVector](v)
          }
          .toSeq

        // TODO: Should have a constructor for key-value pairs.
        import EthereumNodeRecord.byteOrdering

        EthereumNodeRecord(
          rlp.decode[Signature](signature),
          EthereumNodeRecord.Content(
            seq,
            SortedMap(attrs: _*)
          )
        )
      }
    )

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

  implicit def payloadCodec: Codec[Payload] =
    Codec[Payload](
      (payload: Payload) => {
        val (packetType, packetData) =
          payload match {
            case x: Payload.Ping => 0x01 -> rlp.encode(x)
            case x: Payload.Pong => 0x02 -> rlp.encode(x)
            case x: Payload.FindNode => 0x03 -> rlp.encode(x)
            case x: Payload.Neighbors => 0x04 -> rlp.encode(x)
            case x: Payload.ENRRequest => 0x05 -> rlp.encode(x)
            case x: Payload.ENRResponse => 0x06 -> rlp.encode(x)
          }

        Attempt.successful(BitVector(packetType.toByte +: packetData))
      },
      (bits: BitVector) => {
        bits.consumeThen(8)(
          err => Attempt.failure(Err(err)),
          (head, tail) => {
            val packetType: Byte = head.toByte()
            val packetData: Array[Byte] = tail.toByteArray

            val tryPayload: Try[Payload] = Try {
              packetType match {
                case 0x01 => rlp.decode[Payload.Ping](packetData)
                case 0x02 => rlp.decode[Payload.Pong](packetData)
                case 0x03 => rlp.decode[Payload.FindNode](packetData)
                case 0x04 => rlp.decode[Payload.Neighbors](packetData)
                case 0x05 => rlp.decode[Payload.ENRRequest](packetData)
                case 0x06 => rlp.decode[Payload.ENRResponse](packetData)
                case other => throw new RuntimeException(s"Unknown packet type: ${other}")
              }
            }

            Attempt.fromTry(tryPayload.map(DecodeResult(_, BitVector.empty)))
          }
        )
      }
    )
}
