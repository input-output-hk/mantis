package io.iohk.ethereum.network.discovery.codecs

import io.iohk.scalanet.discovery.ethereum.Node
import io.iohk.scalanet.discovery.ethereum.v4.Payload
import io.iohk.ethereum.rlp.RLPCodec
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicitDerivations._
import scodec.Codec
import java.net.InetAddress

/** RLP codecs based on https://github.com/ethereum/devp2p/blob/master/discv4.md */
object RLPCodecs {

  implicit val policy: DerivationPolicy = DerivationPolicy(omitTrailingOptionals = true)

  implicit val inetAddressRLPCodec: RLPCodec[InetAddress] =
    RLPCodec.instance[InetAddress](
      ip => ip.getAddress,
      // Implicit conversion to `Array[Byte]`
      { case rlp => InetAddress.getByAddress(rlp) }
    )

  implicit val nodeAddressRLPCodec: RLPCodec[Node.Address] =
    RLPCodec[Node.Address](
      deriveLabelledGenericRLPListEncoder,
      deriveLabelledGenericRLPListDecoder
    )

  implicit val pingRLPCodec: RLPCodec[Payload.Ping] =
    RLPCodec[Payload.Ping](
      deriveLabelledGenericRLPListEncoder,
      deriveLabelledGenericRLPListDecoder
    )

  implicit def payloadCodec: Codec[Payload] = ???
}
