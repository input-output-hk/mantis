package io.iohk.ethereum.network.discovery.codecs

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import io.iohk.scalanet.discovery.crypto.PublicKey
import io.iohk.scalanet.discovery.hash.Hash
import io.iohk.scalanet.discovery.ethereum.{Node, EthereumNodeRecord}
import io.iohk.scalanet.discovery.ethereum.v4.{Packet, Payload}
import io.iohk.ethereum.network.discovery.Secp256k1SigAlg
import io.iohk.ethereum.rlp.{RLPList, RLPEncoder, RLPDecoder, RLPValue, RLPEncodeable}
import scodec.Codec
import scodec.bits.BitVector
import java.net.InetAddress
import scala.util.Random
import org.scalactic.Equality
import scala.reflect.ClassTag

class RLPCodecsSpec extends AnyFlatSpec with Matchers {
  import io.iohk.ethereum.rlp.RLPImplicitConversions._
  import io.iohk.ethereum.rlp.RLPImplicits._
  import RLPCodecs._

  implicit val sigalg = new Secp256k1SigAlg()

  implicit val packetCodec: Codec[Packet] =
    Packet.packetCodec(allowDecodeOverMaxPacketSize = false)

  val localhost = InetAddress.getByName("127.0.0.1")

  def randomBytes(n: Int): BitVector = {
    val size = Random.nextInt(n)
    val bytes = Array.ofDim[Byte](size)
    Random.nextBytes(bytes)
    BitVector(bytes)
  }

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

  // The following tests demonstrate what each payload looks like when encoded to RLP,
  // because the auto-derivation makes it opaque.
  abstract class EncodeFixture[T <: Payload: RLPEncoder: ClassTag] {
    // Structrual equality checker for RLPEncodeable.
    // It has different wrappers for items based on whether it was hand crafted or generated
    // by codecs, and the RLPValue has mutable arrays inside.
    implicit val eqRLPList = new Equality[RLPEncodeable] {
      override def areEqual(a: RLPEncodeable, b: Any): Boolean =
        (a, b) match {
          case (a: RLPList, b: RLPList) =>
            a.items.size == b.items.size && a.items.zip(b.items).forall { case (a, b) =>
              areEqual(a, b)
            }
          case (a: RLPValue, b: RLPValue) =>
            a.sameElements(b)
          case _ =>
            false
        }
    }

    def name = implicitly[ClassTag[T]].runtimeClass.getSimpleName

    def p: T
    def e: RLPEncodeable

    def test = RLPEncoder.encode(p) should equal(e)
  }

  val examples = List(
    new EncodeFixture[Payload.Ping] {
      override val p = Payload.Ping(
        version = 4,
        from = Node.Address(localhost, 30000, 40000),
        to = Node.Address(localhost, 30001, 0),
        expiration = System.currentTimeMillis,
        enrSeq = Some(1)
      )

      override val e = RLPList(
        p.version,
        RLPList(p.from.ip, p.from.udpPort, p.from.tcpPort),
        RLPList(p.to.ip, p.to.udpPort, p.to.tcpPort),
        p.expiration,
        p.enrSeq.get
      )
    },
    new EncodeFixture[Payload.Pong] {
      override val p = Payload.Pong(
        to = Node.Address(localhost, 30001, 0),
        pingHash = Hash(randomBytes(32)),
        expiration = System.currentTimeMillis,
        enrSeq = Some(1)
      )

      override val e = RLPList(
        RLPList(
          p.to.ip,
          p.to.udpPort,
          p.to.tcpPort
        ),
        p.pingHash,
        p.expiration,
        p.enrSeq.get
      )
    },
    new EncodeFixture[Payload.FindNode] {
      override val p = Payload.FindNode(
        target = PublicKey(randomBytes(64)),
        expiration = System.currentTimeMillis
      )

      override val e = RLPList(p.target, p.expiration)
    },
    new EncodeFixture[Payload.Neighbors] {
      override val p = Payload.Neighbors(
        nodes = List(
          Node(id = PublicKey(randomBytes(64)), address = Node.Address(localhost, 30001, 40001)),
          Node(id = PublicKey(randomBytes(64)), address = Node.Address(localhost, 30002, 40002))
        ),
        expiration = System.currentTimeMillis
      )

      override val e = RLPList(
        RLPList(
          RLPList(p.nodes(0).address.ip, p.nodes(0).address.udpPort, p.nodes(0).address.tcpPort, p.nodes(0).id),
          RLPList(p.nodes(1).address.ip, p.nodes(1).address.udpPort, p.nodes(1).address.tcpPort, p.nodes(1).id)
        ),
        p.expiration
      )
    },
    new EncodeFixture[Payload.ENRRequest] {
      override val p = Payload.ENRRequest(
        expiration = System.currentTimeMillis
      )

      override val e = RLPList(
        p.expiration
      )
    },
    new EncodeFixture[Payload.ENRResponse] {
      val (publicKey, privateKey) = sigalg.newKeyPair
      val node = Node(
        id = publicKey,
        address = Node.Address(localhost, 30000, 40000)
      )
      val enr = EthereumNodeRecord.fromNode(node, privateKey, seq = 1).require

      override val p = Payload.ENRResponse(
        requestHash = Hash(randomBytes(32)),
        enr = enr
      )

      import EthereumNodeRecord.Keys

      override val e = RLPList(
        p.requestHash,
        RLPList(
          p.enr.signature,
          p.enr.content.seq,
          Keys.id,
          p.enr.content.attrs(Keys.id),
          Keys.ip,
          p.enr.content.attrs(Keys.ip),
          Keys.secp256k1,
          p.enr.content.attrs(Keys.secp256k1),
          Keys.tcp,
          p.enr.content.attrs(Keys.tcp),
          Keys.udp,
          p.enr.content.attrs(Keys.udp)
        )
      )
    }
  )

  examples.foreach { example =>
    it should s"encode the example ${example.name}" in {
      example.test
    }
  }
}
