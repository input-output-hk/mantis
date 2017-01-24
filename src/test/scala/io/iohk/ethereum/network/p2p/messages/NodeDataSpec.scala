package io.iohk.ethereum.network.p2p.messages

import PV63._
import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class NodeDataSpec extends FlatSpec with Matchers {

  val emptyEvmHash: ByteString = ByteString(Hex.decode(""))
  val emptyStorageRoot: ByteString = ByteString(Hex.decode(""))

  val account = Account(nonce = 12, balance = 2000, emptyStorageRoot, emptyEvmHash)
  val leafNode = MptLeaf(ByteString(Hex.decode("")), account)
  val branchNode = MptBranch(Seq(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null), ByteString())
  val extensionNode = MptExtension(ByteString(Hex.decode("")), ByteString(Hex.decode("")))


  val nodeData = NodeData(Seq(Left(leafNode), Left(branchNode), Left(extensionNode), Right(emptyEvmHash), Right(emptyStorageRoot)))


  "NodeData" should "be encoded properly" in {

  }

  "NodeData" should "be decoded properly" in {

  }

  "MptBranch" should "be encoded with empty elements at correct positions" in {

  }
}
