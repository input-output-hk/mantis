package io.iohk.ethereum.network.discovery.codecs

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import io.iohk.scalanet.discovery.ethereum.Node
import io.iohk.scalanet.discovery.ethereum.v4.{Packet, Payload}
import io.iohk.ethereum.rlp.{RLPList, RLPEncoder}
import scodec.Codec
import java.net.InetAddress
import io.iohk.ethereum.rlp.{RLPDecoder, RLPValue}

class RLPCodecsSpec extends AnyFlatSpec with Matchers {

  import RLPCodecs._

  implicit val packetCodec: Codec[Packet] =
    Packet.packetCodec(allowDecodeOverMaxPacketSize = false)

  val localhost = InetAddress.getByName("127.0.0.1")

  behavior of "RLPCodecs"

  it should "encode a Ping with an ENR as 5 items" in {
    val ping = Payload.Ping(
      version = 4,
      from = Node.Address(localhost, 30000, 40000),
      to = Node.Address(localhost, 30001, 0),
      expiration = System.currentTimeMillis,
      enrSeq = Some(1)
    )

    val rlp = RLPEncoder.encode(ping)

    rlp match {
      case list: RLPList =>
        list.items should have size 5
        list.items.last shouldBe an[RLPValue]
      case other =>
        fail(s"Expected RLPList; got $other")
    }

    RLPDecoder.decode[Payload.Ping](rlp) shouldBe ping
  }

  it should "encode a Ping without an ENR as 4 items" in {
    val ping = Payload.Ping(
      version = 4,
      from = Node.Address(localhost, 30000, 40000),
      to = Node.Address(localhost, 30001, 0),
      expiration = System.currentTimeMillis,
      enrSeq = None
    )

    val rlp = RLPEncoder.encode(ping)

    rlp match {
      case list: RLPList =>
        list.items should have size 4
      case other =>
        fail(s"Expected RLPList; got $other")
    }

    RLPDecoder.decode[Payload.Ping](rlp) shouldBe ping
  }
}
