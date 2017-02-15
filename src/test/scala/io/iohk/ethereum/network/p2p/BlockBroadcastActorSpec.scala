package io.iohk.ethereum.network.p2p

import java.net.{InetSocketAddress, URI}

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.{BlockBodiesStorage, BlockHeadersStorage, TotalDifficultyStorage}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV61.NewBlockHashes
import io.iohk.ethereum.network.{Block, BlockBroadcastActor, PeerActor, PeerManagerActor}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockBody, BlockHeaders}

class BlockBroadcastActorSpec extends FlatSpec with Matchers {

  val NumberPeers = 10

  "BlockBroadcastActor" should "subscribe for messages" in {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast

    peerProbe.expectMsg(PeerActor.Subscribe(Set(NewBlock.code, NewBlockHashes.code, BlockHeaders.code, BlockBodies.code)))
  }

  it should "broadcast only blocks that it hasn't yet received (but not to the sending peer)" in {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast

    val newBlock = NewBlock(block.blockHeader, block.blockBody, block.blockHeader.difficulty + blockParent.blockHeader.difficulty)
    blockBroadcast ! PeerActor.MessageReceived(newBlock)

    peerManager.expectMsg(GetPeers)

    peerManager.reply(PeerManagerActor.PeersResponse(allPeers))

    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => p.expectMsg(PeerActor.SendMessage(newBlock)) }
  }

  it should "not broadcast repeated blocks" in {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast

    val newBlock = NewBlock(block.blockHeader, block.blockBody, block.blockHeader.difficulty + blockParent.blockHeader.difficulty)
    blockBroadcast ! PeerActor.MessageReceived(newBlock)

    peerManager.expectMsg(GetPeers)

    //Send a repeated block
    blockBroadcast ! PeerActor.MessageReceived(newBlock)

    peerManager.reply(PeerManagerActor.PeersResponse(allPeers))

    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => p.expectMsg(PeerActor.SendMessage(newBlock)) }

    //Each peer should have only received one block
    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => assert(!p.msgAvailable) }
  }

  it should "not broadcast blocks whose parent is not known" in {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast

    val newBlock = NewBlock(otherBlock.blockHeader, otherBlock.blockBody, otherBlock.blockHeader.difficulty)
    blockBroadcast ! PeerActor.MessageReceived(newBlock)

    Thread.sleep(1000)

    //No message should have been sent
    assert(!peerManager.msgAvailable)
    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => assert(!p.msgAvailable) }
  }

  implicit val system = ActorSystem("PeerActorSpec_System")

  val peerProbe = TestProbe()
  val peer = Peer(new InetSocketAddress("localhost", NumberPeers), peerProbe.ref)
  val peerManager = TestProbe()

  val peersProbes = (0 until NumberPeers).map { _ => TestProbe() }
  val peers: Seq[Peer] = peersProbes.zipWithIndex.map { case (testProbe, i) => Peer(new InetSocketAddress("localhost", i), testProbe.ref) }

  val allPeers = peer +: peers

  //Block whose parent is in storage
  val block = Block(
    BlockHeader(
      parentHash = ByteString(Hex.decode("7d7624494a009676b3da30f967c68623e6d940bc53fb8efbbc23369626ef4fac")),
      ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
      beneficiary = ByteString(Hex.decode("23301ac1e0faa254c0cba8265e54abb20b4bded1")),
      stateRoot = ByteString(Hex.decode("69a98eeeb69be0fb528c4c64485dd570138d45c5daa51f8d7947ffbb166217e5")),
      transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      logsBloom = ByteString(Hex.decode("00" * 256)),
      difficulty = BigInt("982560"),
      number = 1,
      gasLimit = 134086657,
      gasUsed = 0,
      unixTimestamp = 1487079160,
      extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
      mixHash = ByteString(Hex.decode("d556da10ebf4789a9d407133605bc35c5e8ae25d7a6276a4754d95a35c82a301")),
      nonce = ByteString(Hex.decode("5d33e977872dd4ed"))
    ),
    BlockBody(Seq(), Seq())
  )

  val blockParent = Block(
    BlockHeader(
      parentHash = ByteString(Hex.decode("0000000000000000000000000000000000000000000000000000000000000000")),
      ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
      beneficiary = ByteString(Hex.decode("3333333333333333333333333333333333333333")),
      stateRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      logsBloom = ByteString(Hex.decode("00" * 256)),
      difficulty = BigInt("983040"),
      number = 0,
      gasLimit = 134217728,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = ByteString(Hex.decode("00")),
      mixHash = ByteString(Hex.decode("0000000000000000000000000000000000000000000000000000000000000000")),
      nonce = ByteString(Hex.decode("deadbeefdeadbeef"))
    ),
    BlockBody(Seq(), Seq())
  )

  //Block whose parent is not in storage
  val otherBlock = Block(
    BlockHeader(
      parentHash = ByteString(Hex.decode("7f4222dd64e80312230b78abc6ab7d22d8bb6a1c5c11a1e60770e23609ed38f0")),
      ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
      beneficiary = ByteString(Hex.decode("23301ac1e0faa254c0cba8265e54abb20b4bded1")),
      stateRoot = ByteString(Hex.decode("d319cf679058faf6b03ff530537ab754a876c3bbf3b3ed9b7ff1e7841c9d840d")),
      transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
      difficulty = BigInt("982560"),
      number = 3,
      gasLimit = 133824899,
      gasUsed = 0,
      unixTimestamp = 1487079252,
      extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
      mixHash = ByteString(Hex.decode("13964e98af47212c796ed49db93b8cbc251a7447feb1461f34f04665a7bb3989")),
      nonce = ByteString(Hex.decode("3503c4e92f53b371"))
    ),
    BlockBody(Seq(), Seq())
  )

  val blockBroadcast = TestActorRef(BlockBroadcastActor.props(
    peer.ref,
    peerManager.ref,
    new BlockHeadersStorage(EphemDataSource()).put(blockParent.blockHeader.hash, blockParent.blockHeader),
    new BlockBodiesStorage(EphemDataSource()).put(blockParent.blockHeader.hash, blockParent.blockBody),
    new TotalDifficultyStorage(EphemDataSource()).put(blockParent.blockHeader.hash, blockParent.blockHeader.difficulty)
  ))

}
