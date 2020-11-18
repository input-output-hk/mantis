package io.iohk.ethereum.network.discovery.codecs

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import io.iohk.scalanet.discovery.ethereum.{Node, EthereumNodeRecord}
import io.iohk.scalanet.discovery.ethereum.v4.Payload.ENRResponse
import io.iohk.scalanet.discovery.crypto.{SigAlg, PrivateKey, PublicKey}
import io.iohk.scalanet.discovery.hash.{Hash, Keccak256}
import io.iohk.ethereum.network.discovery.Secp256k1SigAlg
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.{RLPList, RLPEncoder, RLPValue}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import scodec.bits.{ByteVector, HexStringSyntax}
import java.net.InetAddress
import io.iohk.ethereum.rlp.RLPEncodeable
import io.iohk.ethereum.rlp.RLPDecoder
import scala.language.implicitConversions

class ENRCodecsSpec extends AnyFlatSpec with Matchers {

  import RLPCodecs._

  implicit val sigalg: SigAlg = new Secp256k1SigAlg

  val localhost = InetAddress.getByName("127.0.0.1")

  behavior of "RLPCodecs with ENR"

  // https://github.com/ethereum/devp2p/blob/master/enr.md#test-vectors
  val nodeId = Hash(hex"a448f24c6d18e575453db13171562b71999873db5b286df957af199ec94617f7".toBitVector)
  val privateKey = PrivateKey(hex"b71c71a67e1177ad4e901695e1b4b9ee17ae16c6668d313eac2f96dbcda3f291".toBitVector)
  val publicKey = sigalg.toPublicKey(privateKey)
  val enrString =
    "enr:-IS4QHCYrYZbAKWCBRlAy5zzaDZXJBGkcnh4MHcBFZntXNFrdvJjX04jRzjzCBOonrkTfj499SZuOh8R33Ls8RRcy5wBgmlkgnY0gmlwhH8AAAGJc2VjcDI1NmsxoQPKY0yuDUmstAHYpMa2_oxVtw0RW_QAdpzBQA8yWM0xOIN1ZHCCdl8"

  val enrRLP = RLPList(
    hex"7098ad865b00a582051940cb9cf36836572411a47278783077011599ed5cd16b76f2635f4e234738f30813a89eb9137e3e3df5266e3a1f11df72ecf1145ccb9c",
    1L,
    "id",
    "v4",
    "ip",
    hex"7f000001",
    "secp256k1",
    hex"03ca634cae0d49acb401d8a4c6b6fe8c55b70d115bf400769cc1400f3258cd3138",
    "udp",
    hex"765f"
  )

  it should "encode the test ENR RLP to the expected bytes" in {
    // https://github.com/ethereum/devp2p/blob/master/enr.md#text-encoding
    val enrBytes = ByteVector.fromBase64(enrString.stripPrefix("enr:"), scodec.bits.Bases.Alphabets.Base64Url).get
    // Check that the RLP really serializes to that string representation.
    ByteVector(rlp.encode(enrRLP)) shouldBe enrBytes
  }

  it should "encode a Node to the expected RLP structure" in {
    // Construct a Node which should give us the same results.
    // Unfortunately there's no option to omit the TCP port from it :(
    val node = Node(
      id = publicKey,
      address = Node.Address(localhost, udpPort = 30303, tcpPort = 0)
    )

    val enr = EthereumNodeRecord.fromNode(node, privateKey, seq = 1).require

    val rlpStructureFromNode = RLPEncoder.encode(enr)

    rlpStructureFromNode match {
      case list: RLPList =>
        def compare(a: Iterable[RLPEncodeable], b: Iterable[RLPEncodeable]) = {
          // .toString hack used because RLPValue has mutable arrays in it where equality doesn't work.
          val encoded = a.map(_.toString).toList
          val expected = b.map(_.toString).toList
          encoded should contain theSameElementsInOrderAs expected
        }

        // Ignoring the signature, taking items up to where "tcp" would be.
        compare(list.items.drop(1).take(7), enrRLP.items.drop(1).take(7))

        // The example is encoded differently because it uses the minimum
        // length for the port, whereas the one in Scalanet just converts the
        // Int to BigEndian bytes and includes them in the attributes.
        //
        // This should be fine from signature checking perspective as
        // that is still carried out on the byte content of the attribute map,
        // and nodes should be able to deserialize what we send.
        //
        // We might have problems if someone sends 0 for port as that
        // would serialize to an empty byte array in RLP. But we can't
        // address such nodes anyway, so it should be enough to check
        // that we can deal with the shorter bytes (done below in a test).
        compare(
          list.items.drop(8),
          RLPList(
            "tcp",
            hex"00000000",
            "udp",
            hex"0000765f"
          ).items
        )

      case other =>
        fail(s"Unexpected RLP structure $other")
    }
  }

  it should "decode the address from the example ENR" in {
    // We need the TCP port, so let's inject one.
    val enrRLPWithTCP = enrRLP ++ RLPList("tcp", hex"7660")
    val enr = RLPDecoder.decode[EthereumNodeRecord](enrRLPWithTCP)

    Node.Address.fromEnr(enr) match {
      case Some(address) =>
        address.ip shouldBe localhost
        address.udpPort shouldBe 30303
        address.tcpPort shouldBe 30304
      case None =>
        fail("Couldn't extract the node address")
    }
  }

  it should "verify the signature of the example ENR" in {
    val publicKey = sigalg.toPublicKey(privateKey)
    val enr = RLPDecoder.decode[EthereumNodeRecord](enrRLP)
    EthereumNodeRecord.validateSignature(enr, publicKey).require shouldBe true
  }

  it should "verify that the node ID in the example is the hash of the public key" in {
    // This is what we use in Kademlia, but the node ID in the wire protocol
    // should be the 64 byte public key, at least I thought so based on the spec.
    Keccak256(publicKey) shouldBe nodeId
  }

  it should "handle arbitrary key-value pairs" in {
    implicit def `BitVector => Array[Byte]`(b: ByteVector): Array[Byte] = b.toArray

    // This is a record returned by one of the nodes on the mordor testnet.
    val enrResponseWithNonByteValue = RLPList(
      RLPValue(hex"b800b3f96bc648c5008b3734f591aedfcb26fff3f709ccc43d70de90ed10ab47"),
      RLPList(
        RLPValue(
          hex"3a5b30bccf05526253d5145239c6b07073dfc6747b54a1ae9ee81da7b2ac9caf72e82e90c9d47ca909a1a6348dde6c79cd7df59fd281c246d01a66913551609d"
        ),
        RLPValue(hex"31"),
        // key: "eth"
        RLPValue(hex"657468"),
        // not an RLPValue, unlike the rest which are bytes
        RLPList(
          RLPList(RLPValue(hex"66b5c286"), RLPValue(Array.empty))
        ),
        RLPValue(hex"6964"),
        RLPValue(hex"7634"),
        RLPValue(hex"6970"),
        RLPValue(hex"339ebf2b"),
        RLPValue(hex"736563703235366b31"),
        RLPValue(hex"0215b6ae4e9e18772f297c90d83645b0fbdb56667ce2d747d6d575b21d7b60c2d3"),
        RLPValue(hex"746370"),
        RLPValue(hex"a113"),
        RLPValue(hex"756470"),
        RLPValue(hex"a113")
      )
    )

    val enr = RLPDecoder.decode[ENRResponse](enrResponseWithNonByteValue).enr

    enr.content.attrs should have size 6

    // We have to be able to reserialize the whole signed content,
    // otherwise we won't be able to verify the signature.
    val publicKey = PublicKey(enr.content.attrs(EthereumNodeRecord.Keys.secp256k1).toBitVector)
    EthereumNodeRecord.validateSignature(enr, publicKey).require shouldBe true

    val address = Node.Address.fromEnr(enr).get
    address.tcpPort should be > 0
    address.udpPort should be > 0
  }
}
