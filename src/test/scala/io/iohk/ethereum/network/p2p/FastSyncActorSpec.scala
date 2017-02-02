package io.iohk.ethereum.network.p2p

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.network.PeerActor.MessageReceived
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, GetReceipts, NodeData, Receipts}
import io.iohk.ethereum.network.{FastSyncActor, NodeInfo, PeerActor}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class FastSyncActorSpec extends FlatSpec with Matchers {


  "FastSyncActor" should "subscribe for messages and ask for basic info" in new TestSetup {
    //when
    fastSync ! FastSyncActor.StartSync(targetBlockHash)

    //then
    peer.expectMsg(PeerActor.Subscribe(Set(NodeData.code, Receipts.code, BlockBodies.code, BlockHeaders.code)))
    peer.expectMsgClass(classOf[PeerActor.SendMessage[GetBlockHeaders]])

    peer.reply(MessageReceived(BlockHeaders(Seq(targetBlockHeader))))

    peer.expectMsg(PeerActor.SendMessage(GetNodeData(Seq(targetBlockHeader.stateRoot))))
    peer.expectMsgClass(classOf[PeerActor.SendMessage[GetBlockHeaders]])

  }

  "FastSyncActor" should "recursively process chain" in new TestSetup {
    // before
    val responseBlockHeaders = Seq(
      genesisBlockHeader,
      genesisBlockHeader.copy(number = 1),
      genesisBlockHeader.copy(number = 2),
      genesisBlockHeader.copy(number = 3))
    val blockHashes: Seq[ByteString] = responseBlockHeaders.map(_.blockHash)

    fastSync ! FastSyncActor.StartSync(targetBlockHash)

    peer.expectMsgClass(classOf[PeerActor.Subscribe])
    peer.expectMsgClass(classOf[PeerActor.SendMessage[GetBlockHeaders]])

    peer.reply(MessageReceived(BlockHeaders(Seq(targetBlockHeader))))

    peer.expectMsgClass(classOf[PeerActor.SendMessage[GetNodeData]])
    peer.expectMsgClass(classOf[PeerActor.SendMessage[GetBlockHeaders]])

    //when
    peer.reply(MessageReceived(BlockHeaders(responseBlockHeaders)))

    //then
    peer.expectMsg(PeerActor.SendMessage(GetBlockBodies(blockHashes)))
    peer.expectMsg(PeerActor.SendMessage(GetReceipts(blockHashes)))
  }

  trait TestSetup {
    val targetBlockHash = ByteString(Hex.decode("ca2b65cf841b7acc2548977ad69a3e118940d0934cdbf2d3645c44bdf5023465"))
    val targetBlockHeader = BlockHeader(
      parentHash = ByteString(Hex.decode("952fe52067413118cd8a69b9436a86ebefe6e9e498378348dbdf2fdc42045f71")),
      ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
      beneficiary = ByteString(Hex.decode("4bb96091ee9d802ed039c4d1a5f6216f90f81b01")),
      stateRoot = ByteString(Hex.decode("88b3593dc60b1bf27a08da97e15b7bb353d3239427e60a18042a685e38abd37d")),
      transactionsRoot = ByteString(Hex.decode("f3f6b2e6e1ac8d58780deebb4c9d139c89e52a6255bd9d9183c52a7afbb7bfd6")),
      receiptsRoot = ByteString(Hex.decode("e9dab9e32635c254966fbb01b61e74f0d66ce16208bcca497fde96eedc53a012")),
      logsBloom = ByteString(Array.fill[Byte](256)(0)),
      difficulty = BigInt("111131897370924"),
      number = BigInt("3025678"),
      gasLimit = BigInt("4007810"),
      gasUsed = BigInt("340142"),
      unixTimestamp = 1484840522,
      extraData = ByteString(Hex.decode("61736961312e657468706f6f6c2e6f7267")),
      mixHash = ByteString(Hex.decode("9a983833b7b0e65d4e3bdac935257894f136845435d15619baae13d3e4f60e4c")),
      nonce = ByteString(Hex.decode("f81fcf00002150c8"))
    )

    val genesisBlockHeader = BlockHeader(
      parentHash = ByteString(Hex.decode("0000000000000000000000000000000000000000000000000000000000000000")),
      ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
      beneficiary = ByteString(Hex.decode("0000000000000000000000000000000000000000")),
      stateRoot = ByteString(Hex.decode("d7f8974fb5ac78d9ac099b9ad5018bedc2ce0a72dad1827a1709da30580f0544")),
      transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      logsBloom = ByteString(Array.fill[Byte](256)(0)),
      difficulty = BigInt("0"),
      number = BigInt("5000"),
      gasLimit = BigInt("0"),
      gasUsed = BigInt("0"),
      unixTimestamp = 0,
      extraData = ByteString(Hex.decode("11bbe8db4e347b4e8c937c1c8370e4b5ed33adb3db69cbdb7a38e1e50b1b82fa")),
      mixHash = ByteString(Hex.decode("0000000000000000000000000000000000000000000000000000000000000000")),
      nonce = ByteString(Hex.decode("0000000000000042"))
    )

    implicit val system = ActorSystem("PeerActorSpec_System")
    val nodeInfo = NodeInfo(crypto.generateKeyPair(), new InetSocketAddress("127.0.0.1", 1))

    val peer = TestProbe()
    val fastSync = TestActorRef(FastSyncActor.props(peer.ref))
  }

}
