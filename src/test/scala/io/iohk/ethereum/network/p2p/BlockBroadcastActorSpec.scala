package io.iohk.ethereum.network.p2p

import java.net.InetSocketAddress

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.agent.Agent
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.PeerActor.SendMessage
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62
import io.iohk.ethereum.network.{BlockBroadcastActor, PeerActor, PeerManagerActor}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.utils.{BlockchainStatus, NodeStatus, ServerStatus}

class BlockBroadcastActorSpec extends FlatSpec with Matchers {

  val PeersNumber = 5

  "BlockBroadcastActor" should "subscribe for messages" in new TestSetup {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast

    peerProbe.expectMsg(PeerActor.Subscribe(Set(NewBlock.code, PV62.NewBlockHashes.code, BlockHeaders.code, BlockBodies.code)))
  }

  it should "broadcast new blocks received (but not to the sending peer)" in new TestSetup {
    peerProbe.send(blockBroadcast, BlockBroadcastActor.StartBlockBroadcast)
    peerProbe.expectMsgClass(classOf[PeerActor.Subscribe])

    val newBlock = NewBlock(block, blockTD)
    peerProbe.send(blockBroadcast, PeerActor.MessageReceived(newBlock))

    peerManager.expectMsg(GetPeers)

    peerManager.reply(PeerManagerActor.PeersResponse(allPeers))

    respondToGetMaxBlockNumberMsgs()

    peerProbe.expectNoMsg()
    peersProbes.foreach { p => p.expectMsg(PeerActor.SendMessage(newBlock)) }
  }

  it should "not broadcast repeated blocks" in new TestSetup {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast
    peerProbe.expectMsgClass(classOf[PeerActor.Subscribe])

    val newBlock = NewBlock(block, blockTD)
    peerProbe.send(blockBroadcast, PeerActor.MessageReceived(newBlock))

    peerManager.expectMsg(GetPeers)

    //Send a repeated block
    peerProbe.send(blockBroadcast, PeerActor.MessageReceived(newBlock))

    peerManager.reply(PeerManagerActor.PeersResponse(allPeers))

    respondToGetMaxBlockNumberMsgs()

    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => p.expectMsg(PeerActor.SendMessage(newBlock)) }

    //Each peer should have only received one block
    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => assert(!p.msgAvailable) }
  }

  it should "not broadcast blocks whose parent is not known" in new TestSetup {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast
    peerProbe.expectMsgClass(classOf[PeerActor.Subscribe])

    val newBlock = NewBlock(otherBlock, otherBlock.header.difficulty)
    peerProbe.send(blockBroadcast, PeerActor.MessageReceived(newBlock))

    time.advance(1.seconds)

    //No message should have been sent
    assert(!peerManager.msgAvailable)
    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => assert(!p.msgAvailable) }
  }

  it should "request and broadcast a message with a new block hash" in new TestSetup {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast
    peerProbe.expectMsgClass(classOf[PeerActor.Subscribe])

    val newBlockHash = PV62.NewBlockHashes(Seq(BlockHash(block.header.hash, block.header.number)))
    peerProbe.send(blockBroadcast, PeerActor.MessageReceived(newBlockHash))

    val reqHeaderMsg: SendMessage[GetBlockHeaders] = peerProbe.receiveN(1).head.asInstanceOf[SendMessage[GetBlockHeaders]]
    reqHeaderMsg.message.block shouldBe Right(block.header.hash)
    peerProbe.reply(PeerActor.MessageReceived(BlockHeaders(Seq(block.header))))

    val reqBodyMsg: SendMessage[GetBlockBodies] = peerProbe.receiveN(1).head.asInstanceOf[SendMessage[GetBlockBodies]]
    reqBodyMsg.message.hashes shouldBe Seq(block.header.hash)
    peerProbe.reply(PeerActor.MessageReceived(BlockBodies(Seq(block.body))))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(PeerManagerActor.PeersResponse(allPeers))

    respondToGetMaxBlockNumberMsgs()

    val expectedNewBlock = NewBlock(block, blockTD)
    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => p.expectMsg(PeerActor.SendMessage(expectedNewBlock)) }
  }

  it should "request and broadcast a message with multiple new block hashes" in new TestSetup {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast
    peerProbe.expectMsgClass(classOf[PeerActor.Subscribe])

    val newBlockHashes = PV62.NewBlockHashes(Seq(
      PV62.BlockHash(block.header.hash, block.header.number),
      PV62.BlockHash(blockSon.header.hash, blockSon.header.number)))
    peerProbe.send(blockBroadcast, PeerActor.MessageReceived(newBlockHashes))

    val reqHeadersMsg: Seq[SendMessage[GetBlockHeaders]] = peerProbe.receiveN(2).asInstanceOf[Seq[SendMessage[GetBlockHeaders]]]
    reqHeadersMsg(0).message.block shouldBe Right(block.header.hash)
    reqHeadersMsg(1).message.block shouldBe Right(blockSon.header.hash)

    peerProbe.reply(PeerActor.MessageReceived(BlockHeaders(Seq(block.header))))
    peerProbe.reply(PeerActor.MessageReceived(BlockHeaders(Seq(blockSon.header))))

    val reqBodiesMsg: Seq[SendMessage[GetBlockBodies]] = peerProbe.receiveN(2).asInstanceOf[Seq[SendMessage[GetBlockBodies]]]
    reqBodiesMsg(0).message.hashes shouldBe Seq(block.header.hash)
    reqBodiesMsg(1).message.hashes shouldBe Seq(blockSon.header.hash)

    peerProbe.reply(PeerActor.MessageReceived(BlockBodies(Seq(block.body))))
    peerProbe.reply(PeerActor.MessageReceived(BlockBodies(Seq(blockSon.body))))

    peerManager.expectMsg(GetPeers)
    peerManager.reply(PeerManagerActor.PeersResponse(allPeers))

    respondToGetMaxBlockNumberMsgs()

    val expectedNewBlock = NewBlock(block, blockTD)
    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => p.expectMsg(PeerActor.SendMessage(expectedNewBlock)) }

    val expectedNewBlockSon = NewBlock(blockSon, blockSonTD)
    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => p.expectMsg(PeerActor.SendMessage(expectedNewBlockSon)) }
  }

  it should "not broadcast an invalid block" in new TestSetup {
    blockBroadcast ! BlockBroadcastActor.StartBlockBroadcast
    peerProbe.expectMsgClass(classOf[PeerActor.Subscribe])

    val parentNumber: BigInt = blockParent.header.number
    val invalidBlock: Block = block.copy(header = block.header.copy(number = parentNumber))

    val newBlock = NewBlock(invalidBlock, blockTD)

    peerProbe.send(blockBroadcast, PeerActor.MessageReceived(newBlock))

    time.advance(1.seconds)

    //No message should have been sent
    assert(!peerManager.msgAvailable)
    assert(!peerProbe.msgAvailable)
    peersProbes.foreach { p => assert(!p.msgAvailable) }
  }

  trait TestSetup extends BlockUtil with EphemBlockchainTestSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")

    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      blockchainStatus = BlockchainStatus(0, ByteString("changeme"), 0))

    val nodeStatusHolder = Agent(nodeStatus)

    val time = new VirtualTime

    val peerProbe = TestProbe()
    val peer = Peer(new InetSocketAddress("localhost", PeersNumber), peerProbe.ref)
    val peerManager = TestProbe()

    val peersProbes: Seq[TestProbe] = (0 until PeersNumber).map { _ => TestProbe() }
    val peers: Seq[Peer] = peersProbes.zipWithIndex.map { case (testProbe, i) => Peer(new InetSocketAddress("localhost", i), testProbe.ref) }

    val allPeers: Seq[Peer] = peer +: peers

    val blockBroadcast = TestActorRef(BlockBroadcastActor.props(
      nodeStatusHolder,
      peer.ref,
      peerManager.ref,
      blockchain
    ))

    //Blockchain setup
    blockchain.save(blockParent)
    blockchain.save(blockParent.header.hash, blockParent.header.difficulty)

    def respondToGetMaxBlockNumberMsgs(): Unit = {
      peersProbes.foreach { p =>
        p.expectMsgClass(classOf[PeerActor.GetMaxBlockNumber])
        p.reply(PeerActor.MaxBlockNumber(0))
      }
    }
  }

  /**
    * Blockchain: blockParent -> block -> blockSon -> ...
    */
  trait BlockUtil {
    //Genesis block, that is already in the blockchain
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
    val blockParentTD: BigInt = blockParent.header.difficulty

    //Son of blockParent
    val block = Block(
      BlockHeader(
        parentHash = ByteString(Hex.decode("7d7624494a009676b3da30f967c68623e6d940bc53fb8efbbc23369626ef4fac")),
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("2cad6e80c7c0b58845fcd71ecad6867c3bd4de20")),
        stateRoot = ByteString(Hex.decode("1aa7610066444010b19fb97153d438d7002cc8358b1b96d7687390310acc524b")),
        transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
        difficulty = BigInt("982560"),
        number = 1,
        gasLimit = 134086657,
        gasUsed = 0,
        unixTimestamp = 1487334141,
        extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
        mixHash = ByteString(Hex.decode("fe0e6834d52d229fbf4d6f24cb948bab9580e125af359597e25441aef425211e")),
        nonce = ByteString(Hex.decode("5fc20258baa0f466"))
      ),
      BlockBody(Seq(), Seq())
    )
    val blockTD: BigInt = blockParentTD + block.header.difficulty

    //Son of block
    val blockSon = Block(
      BlockHeader(
        parentHash = ByteString(Hex.decode("26db59ce5f6b5c43136a75f9c2912adc4129b153182470fd2f52008e1ea590d7")),
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("2cad6e80c7c0b58845fcd71ecad6867c3bd4de20")),
        stateRoot = ByteString(Hex.decode("3cbd9c25ad1bbe76e29b02fe034cb4f7b0835f6b4f310250e3060460259dde42")),
        transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
        difficulty = BigInt("982081"),
        number = 2,
        gasLimit = 133955714,
        gasUsed = 0,
        unixTimestamp = 1487334237,
        extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
        mixHash = ByteString(Hex.decode("8e004c27a58a7eb435a78b9820d11ecc2b13cd8df7ee20bc36d90a16ae5cebd4")),
        nonce = ByteString(Hex.decode("05cf8a589f74cb79"))
      ),
      BlockBody(Seq(), Seq())
    )
    val blockSonTD: BigInt = blockTD + blockSon.header.difficulty

    //Block whose parent is not in storage
    val otherBlock = Block(
      BlockHeader(
        parentHash = ByteString(Hex.decode("98352d9c1300bd82334cb3e5034c3ec622d437963f55cf5a00a49642806c2f32")),
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("2cad6e80c7c0b58845fcd71ecad6867c3bd4de20")),
        stateRoot = ByteString(Hex.decode("9b56d589ad6a12373e212fdb6cb4f64d1d7745aea551c7116c665a81a31f9492")),
        transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
        difficulty = BigInt("985919"),
        number = 10,
        gasLimit = 132912767,
        gasUsed = 0,
        unixTimestamp = 1487334256,
        extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
        mixHash = ByteString(Hex.decode("ea0dec34a635401af44f5245a77b2cd838345615c555c322a3001df4dd0505fe")),
        nonce = ByteString(Hex.decode("60d53a11c10d46fb"))
      ),
      BlockBody(Seq(), Seq())
    )
  }

}
