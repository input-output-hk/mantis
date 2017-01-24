package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.mpt.HexPrefix.{encode => hpEncode, bytesToNibbles}
import io.iohk.ethereum.network.p2p.Message.{PV63 => constantPV63, decode => msgDecode}
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class NodeDataSpec extends FlatSpec with Matchers {

  val emptyEvmHash: ByteString = ByteString(Hex.decode("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))
  val emptyStorageRoot: ByteString = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))

  val accountNonce = 12
  val accountBalance = 2000

  val exampleNibbles = ByteString(bytesToNibbles(Hex.decode("ffddaa")))
  val exampleHash = ByteString(sha3(Hex.decode("abcd")))

  val account = Account(accountNonce, accountBalance, emptyStorageRoot, emptyEvmHash)
  val encodedAccount = RLPList(accountNonce, accountBalance, emptyStorageRoot.toArray[Byte], emptyEvmHash.toArray[Byte])

  val leafNode = MptLeaf(exampleNibbles, account)
  val encodedLeafNode = RLPList(hpEncode(exampleNibbles.toArray[Byte], isLeaf = true), encode(encodedAccount))

  val branchNode = MptBranch(
    (Seq.fill[ByteString](3)(ByteString.empty) :+ exampleHash) ++
      (Seq.fill[ByteString](6)(ByteString.empty) :+ exampleHash) ++
      Seq.fill[ByteString](5)(ByteString.empty), ByteString())

  val encodedBranchNode = RLPList(
    (Seq.fill[RLPValue](3)(RLPValue(Array.emptyByteArray)) :+ RLPValue(exampleHash.toArray[Byte])) ++
      (Seq.fill[RLPValue](6)(RLPValue(Array.emptyByteArray)) :+ RLPValue(exampleHash.toArray[Byte])) ++
      (Seq.fill[RLPValue](5)(RLPValue(Array.emptyByteArray)) :+ RLPValue(Array.emptyByteArray)): _*)

  val extensionNode = MptExtension(exampleNibbles, exampleHash)
  val encodedExtensionNode = RLPList(RLPValue(hpEncode(exampleNibbles.toArray[Byte], isLeaf = false)), RLPValue(exampleHash.toArray[Byte]))

  val nodeData = NodeData(Seq(
    Left(leafNode),
    Left(branchNode),
    Left(extensionNode),
    Right(emptyEvmHash),
    Right(emptyStorageRoot)))

  val encodedNodeData = RLPList(
    RLPValue(encode(encodedLeafNode)),
    RLPValue(encode(encodedBranchNode)),
    RLPValue(encode(encodedExtensionNode)),
    RLPValue(emptyEvmHash.toArray[Byte]),
    RLPValue(emptyStorageRoot.toArray[Byte]))

  "NodeData" should "be encoded properly" in {
    encode(nodeData) shouldBe encode(encodedNodeData)
  }

  "NodeData" should "be decoded properly" in {
    msgDecode(NodeData.code, encode(encodedNodeData), constantPV63) shouldBe nodeData
  }
}
