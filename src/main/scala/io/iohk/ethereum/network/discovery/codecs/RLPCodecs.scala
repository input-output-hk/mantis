package io.iohk.ethereum.network.discovery.codecs

import io.iohk.scalanet.discovery.ethereum.Node
import io.iohk.scalanet.discovery.ethereum.v4.Payload
import io.iohk.ethereum.rlp.{RLPList, RLPEncodeable, RLPCodec}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import scodec.Codec

object RLPCodecs {

  implicit val nodeAddressRLPCodec: RLPCodec[Node.Address] = ???

  implicit val `Option[RLPEncodeable] => Option[Long]` : Option[RLPEncodeable] => Option[Long] =
    fromOptionalEncodeable[Long]

  implicit val nodeAddressFromEncodeable = fromEncodeable[Node.Address](_)

  implicit val pingRLPCodec: RLPCodec[Payload.Ping] =
    RLPCodec[Payload.Ping](
      { case Payload.Ping(version, from, to, expiration, maybeEnrSeq) =>
        val list = RLPList(version, from, to, expiration, maybeEnrSeq)
        if (maybeEnrSeq.isDefined) list else RLPList(list.items.dropRight(1): _*)
      },
      { case RLPList(version, from, to, expiration, rest @ _*) =>
        Payload.Ping(version, from, to, expiration, enrSeq = rest.headOption)
      }
    )

  implicit val payloadCodec: Codec[Payload] = ???
}
