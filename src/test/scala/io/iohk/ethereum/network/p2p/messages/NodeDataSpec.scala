package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, LeafNode, MptNode}
import io.iohk.ethereum.mpt.HexPrefix.{bytesToNibbles, encode => hpEncode}
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode, _}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder

class NodeDataSpec extends FlatSpec with Matchers {

  import AccountImplicits._

  val emptyEvmHash: ByteString = ByteString(Hex.decode("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))
  val emptyStorageRoot: ByteString = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))

  val accountNonce = 12
  val accountBalance = 2000

  val exampleNibbles = ByteString(bytesToNibbles(Hex.decode("ffddaa")))
  val exampleHash = ByteString(kec256(Hex.decode("ab" * 32)))
  val exampleValue = ByteString(Hex.decode("abcdee"))
  val exampleKey = ByteString(Hex.decode("ffddee"))

  val account = Account(accountNonce, accountBalance, emptyStorageRoot, emptyEvmHash)
  val encodedAccount = RLPList(accountNonce, accountBalance, emptyStorageRoot, emptyEvmHash)

  val leafNode = LeafNode(exampleNibbles, account.toBytes)
  val encodedLeafNode = RLPList(hpEncode(exampleNibbles.toArray[Byte], isLeaf = true), encode(encodedAccount))

  val branchNode = new BranchNode(
    (Seq.fill[Option[Either[ByteString, MptNode]]](3)(None) :+ Some(Left(exampleHash))) ++
      (Seq.fill[Option[Either[ByteString, MptNode]]](6)(None) :+ Some(Left(exampleHash))) ++
      Seq.fill[Option[Either[ByteString, MptNode]]](5)(None), None)

  val encodedBranchNode = RLPList(
    (Seq.fill[RLPValue](3)(RLPValue(Array.emptyByteArray)) :+ (exampleHash: RLPEncodeable)) ++
      (Seq.fill[RLPValue](6)(RLPValue(Array.emptyByteArray)) :+ (exampleHash: RLPEncodeable)) ++
      (Seq.fill[RLPValue](5)(RLPValue(Array.emptyByteArray)) :+ (Array.emptyByteArray: RLPEncodeable)): _*)

  val extensionNode = ExtensionNode(exampleNibbles, Left(exampleHash))
  val encodedExtensionNode = RLPList(hpEncode(exampleNibbles.toArray[Byte], isLeaf = false), RLPValue(exampleHash.toArray[Byte]))

  val nodeData = NodeData(Seq(
    leafNode.toBytes,
    branchNode.toBytes,
    extensionNode.toBytes,
    emptyEvmHash,
    emptyStorageRoot))

  val encodedNodeData = RLPList(
    encode(encodedLeafNode),
    encode(encodedBranchNode),
    encode(encodedExtensionNode),
    emptyEvmHash,
    emptyStorageRoot)

  "NodeData" should "be encoded properly" in {
    (nodeData.toBytes: Array[Byte]) shouldBe encode(encodedNodeData)
  }

  it should "be decoded properly" in {
    val result = EthereumMessageDecoder.fromBytes(NodeData.code, encode(encodedNodeData), Versions.PV63)

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
    EthereumMessageDecoder.fromBytes(NodeData.code, nodeData.toBytes, Versions.PV63) shouldBe nodeData
  }

  it should "decode branch node with values in leafs that looks like RLP list" in {
    //given
    val encodedMptBranch =
      Hex.decode("f84d8080808080de9c32ea07b198667c460bb7d8bc9652f6ffbde7b195d81c17eb614e2b8901808080808080de9c3ffe8cb7f9cebdcb4eca6e682b56ab66f4f45827cf27c11b7f0a91620180808080")

    val decodedMptBranch =
      new BranchNode(Seq(
        None,
        None,
        None,
        None,
        None,
        Some(Right(LeafNode(
          key = ByteString(Hex.decode("020e0a00070b0109080606070c0406000b0b070d080b0c090605020f060f0f0b0d0e070b0109050d08010c01070e0b0601040e020b0809")),
          value = ByteString(1)))),
        None,
        None,
        None,
        None,
        None,
        None,
        Some(Right(LeafNode(
          key = ByteString(Hex.decode("0f0f0e080c0b070f090c0e0b0d0c0b040e0c0a060e0608020b05060a0b06060f040f04050802070c0f02070c01010b070f000a09010602")),
          value = ByteString(1)))),
        None,
        None,
        None
      ), None)

    //when
    val result: MptNode = encodedMptBranch.toMptNode

    //then
    result shouldBe decodedMptBranch
  }

  it should "obtain the same value when decoding and encoding an encoded node" in {
    //given
    val encodedMptBranch =
      Hex.decode("f84d8080808080de9c32ea07b198667c460bb7d8bc9652f6ffbde7b195d81c17eb614e2b8901808080808080de9c3ffe8cb7f9cebdcb4eca6e682b56ab66f4f45827cf27c11b7f0a91620180808080")

    //when
    val result: MptNode = encodedMptBranch.toMptNode

    //then
    (result.toBytes: Array[Byte]) shouldBe encodedMptBranch //This fails
  }
}
