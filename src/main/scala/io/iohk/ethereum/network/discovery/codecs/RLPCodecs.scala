package io.iohk.ethereum.network.discovery.codecs

import io.iohk.scalanet.discovery.ethereum.Node
import io.iohk.scalanet.discovery.ethereum.v4.Payload
import io.iohk.ethereum.rlp.{RLPList, RLPEncodeable, RLPCodec, RLPEncoder, RLPDecoder}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicitDerivations._
import scodec.Codec
import java.net.InetAddress

/** RLP codecs based on https://github.com/ethereum/devp2p/blob/master/discv4.md */
object RLPCodecs {

  implicit val policy: DerivationPolicy = DerivationPolicy(omitTrailingOptionals = true)

  implicit val nodeAddressRLPCodec: RLPCodec[Node.Address] =
    RLPCodec[Node.Address](
      { case Node.Address(ip, udpPort, tcpPort) =>
        RLPList(ip.getAddress, udpPort, tcpPort)
      },
      { case RLPList(ip, udpPort, tcpPort, _*) =>
        Node.Address(InetAddress.getByAddress(ip), udpPort, tcpPort)
      }
    )

  implicit val `Option[RLPEncodeable] => Option[Long]` : Option[RLPEncodeable] => Option[Long] =
    fromOptionalEncodeable[Long]

  implicit val nodeAddressFromEncodeable = fromEncodeable[Node.Address](_)

  implicit val pingRLPEncoder: RLPEncoder[Payload.Ping] =
    deriveLabelledGenericRLPListEncoder

  implicit val pingRLPDecoder: RLPDecoder[Payload.Ping] =
    deriveLabelledGenericRLPListDecoder

  implicit def payloadCodec: Codec[Payload] = ???
}
