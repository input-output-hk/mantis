package io.iohk.ethereum.network.p2p

import java.net.{InetSocketAddress, URI}

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerActor.SendMessage
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.{Block, BlockBroadcastActor, PeerActor}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

class BlockBroadcastActorSpec extends FlatSpec with Matchers {

  val NumberPeers = 10

  "BlockBroadcastActor" should "subscribe for messages" in {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast

    peer.expectMsg(PeerActor.Subscribe(Set(NewBlock.code)))
  }

  it should "broadcast any sent blocks to it" in {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast

    val newBlock = NewBlock(block.blockHeader, block.blockBody, BigInt("989772"))
    blockBroadcast ! PeerActor.MessageReceived(newBlock)

    peerManager.expectMsg(GetPeers)

    peerManager.reply(peers)

    peersProbes.foreach{ peer =>
      peer.expectMsg(PeerActor.SendMessage(newBlock))
    }
  }

  implicit val system = ActorSystem("PeerActorSpec_System")

  val peer = TestProbe()
  val peerManager = TestProbe()
  val blockBroadcast = TestActorRef(BlockBroadcastActor.props(peer.ref, peerManager.ref))

  val block = Block(
    BlockHeader(
      parentHash = ByteString(Hex.decode("d882d5c210bab4cb7ef0b9f3dc2130cb680959afcd9a8f9bf83ee6f13e2f9da3")),
      ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
      beneficiary = ByteString(Hex.decode("95f484419881c6e9b6de7fb3f8ad03763bd49a89")),
      stateRoot = ByteString(Hex.decode("634a2b20c9e02afdda7157afe384306c5acc4fb9c09b45dc0203c0fbb2fed0e6")),
      transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      logsBloom = ByteString(Hex.decode("00" * 256)),
      difficulty = BigInt("989772"),
      number = 20,
      gasLimit = 131620495,
      gasUsed = 0,
      unixTimestamp = 1486752441,
      extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
      mixHash = ByteString(Hex.decode("6bc729364c9b682cfa923ba9480367ebdfa2a9bca2a652fe975e8d5958f696dd")),
      nonce = ByteString(Hex.decode("797a8f3a494f937b"))
    ),
    BlockBody(Seq(), Seq())
  )

  val peersProbes = (0 until NumberPeers).map{_ => TestProbe()}
  val peers = peersProbes.zipWithIndex.map{ case (testProbe, i) =>
    i.toString -> Peer(new InetSocketAddress("localhost", i), testProbe.ref)
  }.toMap
}
