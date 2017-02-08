package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.mpt.HexPrefix.{bytesToNibbles, encode => hpEncode}
import io.iohk.ethereum.network.p2p.Message.{PV63 => constantPV63, decode => msgDecode}
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.rlp.RLPImplicits._
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class NodeDataSpec extends FlatSpec with Matchers {

  import AccountImplicits._

  val emptyEvmHash: ByteString = ByteString(Hex.decode("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))
  val emptyStorageRoot: ByteString = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))

  val accountNonce = 12
  val accountBalance = 2000

  val exampleNibbles = ByteString(bytesToNibbles(Hex.decode("ffddaa")))
  val exampleHash = ByteString(sha3(Hex.decode("abcd")))
  val exampleValue = ByteString(Hex.decode("abcdee"))

  val account = Account(accountNonce, accountBalance, emptyStorageRoot, emptyEvmHash)
  val encodedAccount = RLPList(accountNonce, accountBalance, emptyStorageRoot, emptyEvmHash)

  val leafNode = MptLeaf(exampleNibbles, ByteString(encode(account)))
  val encodedLeafNode = RLPList(hpEncode(exampleNibbles.toArray[Byte], isLeaf = true), encode(encodedAccount))

  val branchNode = MptBranch(
    (Seq.fill[Either[MptHash, MptValue]](3)(Left(MptHash(ByteString.empty))) :+ Left(MptHash(exampleHash))) ++
      (Seq.fill[Either[MptHash, MptValue]](6)(Left(MptHash(ByteString.empty))) :+ Left(MptHash(exampleHash))) ++
      Seq.fill[Either[MptHash, MptValue]](5)(Left(MptHash(ByteString.empty))), ByteString())

  val encodedBranchNode = RLPList(
    (Seq.fill[RLPValue](3)(RLPValue(Array.emptyByteArray)) :+ (exampleHash: RLPEncodeable)) ++
      (Seq.fill[RLPValue](6)(RLPValue(Array.emptyByteArray)) :+ (exampleHash: RLPEncodeable)) ++
      (Seq.fill[RLPValue](5)(RLPValue(Array.emptyByteArray)) :+ (Array.emptyByteArray: RLPEncodeable)): _*)

  val extensionNode = MptExtension(exampleNibbles, Right(MptValue(exampleValue)))
  val encodedExtensionNode = RLPList(hpEncode(exampleNibbles.toArray[Byte], isLeaf = false), exampleValue)

  val nodeData = NodeData(Seq(
    ByteString(encode[MptNode](leafNode)),
    ByteString(encode[MptNode](branchNode)),
    ByteString(encode[MptNode](extensionNode)),
    emptyEvmHash,
    emptyStorageRoot))

  val encodedNodeData = RLPList(
    encode(encodedLeafNode),
    encode(encodedBranchNode),
    encode(encodedExtensionNode),
    emptyEvmHash,
    emptyStorageRoot)

  "NodeData" should "be encoded properly" in {
    encode(nodeData) shouldBe encode(encodedNodeData)
  }

  it should "be decoded properly" in {
    val result = msgDecode(NodeData.code, encode(encodedNodeData), constantPV63)

    result match {
      case m: NodeData =>
        m.getMptNode(0) shouldBe leafNode
        m.getMptNode(1) shouldBe branchNode
        m.getMptNode(2) shouldBe extensionNode
      case _ => fail("wrong type")
    }

    result shouldBe nodeData
  }

  it should "be decoded previously encoded value" in {
    msgDecode(NodeData.code, encode(nodeData), constantPV63) shouldBe nodeData
  }
}
