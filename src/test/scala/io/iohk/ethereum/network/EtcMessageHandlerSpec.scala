package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor.{ActorSystem, Props}
import akka.agent.Agent
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum
import io.iohk.ethereum.{Fixtures, crypto}
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Block, BlockHeader, BlockchainImpl, Receipt}
import io.iohk.ethereum.mpt.HexPrefix.bytesToNibbles
import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.MessageHandler.MessageAction.{IgnoreMessage, TransmitMessage}
import io.iohk.ethereum.network.MessageHandler.MessageHandlingResult
import io.iohk.ethereum.network.PeerActor.DisconnectPeer
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, Peer, PeerConfiguration}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.rlp.encode
import io.iohk.ethereum.utils.{BlockchainConfig, Config, NodeStatus, ServerStatus}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.language.postfixOps
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
class EtcMessageHandlerSpec extends FlatSpec with Matchers {

  it should "return Receipts for block hashes" in new TestSetup {
    //given
    val receiptsHashes = Seq(
      ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
      ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")))

    val receipts: Seq[Seq[Receipt]] = Seq(Seq(),Seq())

    blockchain.save(receiptsHashes(0), receipts(0))
    blockchain.save(receiptsHashes(1), receipts(1))

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetReceipts(receiptsHashes))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(Receipts(receipts)))
  }

  it should "return BlockBodies for block hashes" in new TestSetup {
    //given
    val blockBodiesHashes = Seq(
      ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
      ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")))

    val blockBodies = Seq(baseBlockBody,baseBlockBody)

    blockchain.save(blockBodiesHashes(0), blockBodies(0))
    blockchain.save(blockBodiesHashes(1), blockBodies(1))

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetBlockBodies(blockBodiesHashes))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(BlockBodies(blockBodies)))
  }

  it should "return block headers by block number" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 4)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)
    blockchain.save(baseBlockHeader.copy(number = 5))
    blockchain.save(baseBlockHeader.copy(number = 6))

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetBlockHeaders(Left(3), 2, 0, reverse = false))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers by block number when response is shorter then what was requested" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 4)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetBlockHeaders(Left(3), 3, 0, reverse = false))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers by block number in reverse order" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 2)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)
    blockchain.save(baseBlockHeader.copy(number = 1))

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetBlockHeaders(Left(3), 2, 0, reverse = true))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers by block hash" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 4)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)
    blockchain.save(baseBlockHeader.copy(number = 5))
    blockchain.save(baseBlockHeader.copy(number = 6))

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetBlockHeaders(Right(firstHeader.hash), 2, 0, reverse = false))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers by block hash when skipping headers" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 5)

    blockchain.save(firstHeader)
    blockchain.save(baseBlockHeader.copy(number = 4))
    blockchain.save(secondHeader)
    blockchain.save(baseBlockHeader.copy(number = 6))
    blockchain.save(baseBlockHeader.copy(number = 7))
    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetBlockHeaders(Right(firstHeader.hash), maxHeaders = 2, skip = 1, reverse = false))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers in reverse when there are skipped blocks" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 1)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetBlockHeaders(Right(firstHeader.hash), 2, 1, reverse = true))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers in reverse when there are skipped blocks and we are asking for blocks before genesis" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 1)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetBlockHeaders(Right(firstHeader.hash), 3, 1, reverse = true))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers in reverse when there are skipped blocks ending at genesis" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 4)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 2)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetBlockHeaders(Right(firstHeader.hash), 4, 1, reverse = true))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader, blockchain.genesisHeader))))
  }

  it should "return evm code for hash" in new TestSetup {
    //given
    val fakeEvmCode = ByteString(Hex.decode("ffddaaffddaaffddaaffddaaffddaa"))
    val evmCodeHash: ByteString = ByteString(ethereum.crypto.kec256(fakeEvmCode.toArray[Byte]))

    blockchain.save(evmCodeHash, fakeEvmCode)

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(GetNodeData(Seq(evmCodeHash)))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(NodeData(Seq(fakeEvmCode))))
  }

  it should "return mptNode for hash" in new TestSetup {
    //given
    val exampleNibbles = ByteString(bytesToNibbles(Hex.decode("ffddaa")))
    val exampleHash = ByteString(Hex.decode("ab"*32))
    val extensionNode: MptNode = MptExtension(exampleNibbles, Left(MptHash(exampleHash)))

    blockchain.save(extensionNode)

    //when
    val MessageHandlingResult(msgHandler, action) = initialMsgHandler.receivingMessage(GetNodeData(Seq(extensionNode.hash)))

    //then
    msgHandler.peerInfo shouldBe initialMsgHandler.peerInfo
    action shouldBe IgnoreMessage
    peerProbe.expectMsg(PeerActor.SendMessage(NodeData(Seq(ByteString(encode(extensionNode))))))
  }

  it should "update max peer when receiving new block" in new TestSetup {
    //given
    val newBlockTD = 300
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 4)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), newBlockTD)

    val secondHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 2)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Nil, Nil)), newBlockTD)

    //when
    val MessageHandlingResult(msgHandler1, action1) =  initialMsgHandler.receivingMessage(firstBlock)
    val MessageHandlingResult(msgHandler2, action2) =  msgHandler1.receivingMessage(secondBlock)

    //then
    val expectedPeerInfo = initialPeerInfo
      .withMaxBlockNumber(initialPeerInfo.maxBlockNumber + 4)
      .withTotalDifficulty(newBlockTD)
    msgHandler2.peerInfo shouldBe expectedPeerInfo
    action1 shouldBe TransmitMessage
    action2 shouldBe TransmitMessage
  }

  it should "update max peer when receiving block header" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 4)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 2)

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(BlockHeaders(Seq(firstHeader, secondHeader, blockchain.genesisHeader)))

    //then
    msgHandler.peerInfo shouldBe initialPeerInfo.withMaxBlockNumber(initialPeerInfo.maxBlockNumber + 4)
    action shouldBe TransmitMessage
  }

  it should "update max peer when receiving new block hashes" in new TestSetup {
    //given
    val firstBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), initialPeerInfo.maxBlockNumber + 2)
    val secondBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), initialPeerInfo.maxBlockNumber + 5)

    //when
    val MessageHandlingResult(msgHandler, action) =
      initialMsgHandler.receivingMessage(NewBlockHashes(Seq(firstBlockHash, secondBlockHash)))

    //then
    msgHandler.peerInfo shouldBe initialPeerInfo.withMaxBlockNumber(initialPeerInfo.maxBlockNumber + 5)
    action shouldBe TransmitMessage
  }

  it should "update max peer when sending new block" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 4)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), 300)

    val secondHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 2)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Nil, Nil)), 300)

    //when
    val MessageHandlingResult(msgHandler1, action1) = initialMsgHandler.sendingMessage(firstBlock)
    val MessageHandlingResult(msgHandler2, action2) = msgHandler1.sendingMessage(secondBlock)

    //then
    msgHandler2.peerInfo shouldBe initialPeerInfo.withMaxBlockNumber(initialPeerInfo.maxBlockNumber + 4)
    action1 shouldBe TransmitMessage
    action2 shouldBe IgnoreMessage //Block should already be known by the peer due to max block known
  }

  it should "update max peer when sending block header" in new TestSetup {
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 4)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = initialPeerInfo.maxBlockNumber + 2)

    //when
    val MessageHandlingResult(msgHandler1, action1) = initialMsgHandler.sendingMessage(BlockHeaders(Seq(firstHeader)))
    val MessageHandlingResult(msgHandler2, action2) = msgHandler1.sendingMessage(BlockHeaders(Seq(secondHeader)))

    //then
    msgHandler2.peerInfo shouldBe initialPeerInfo.withMaxBlockNumber(initialPeerInfo.maxBlockNumber + 4)
    action1 shouldBe TransmitMessage
    action2 shouldBe TransmitMessage
  }

  it should "update max peer when sending new block hashes" in new TestSetup {
    //given
    val firstBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), initialPeerInfo.maxBlockNumber + 2)
    val secondBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), initialPeerInfo.maxBlockNumber + 5)

    //when
    val MessageHandlingResult(msgHandler1, action1) = initialMsgHandler.sendingMessage(NewBlockHashes(Seq(firstBlockHash)))
    val MessageHandlingResult(msgHandler2, action2) = msgHandler1.sendingMessage(NewBlockHashes(Seq(secondBlockHash)))

    //then
    msgHandler2.peerInfo shouldBe initialPeerInfo.withMaxBlockNumber(initialPeerInfo.maxBlockNumber + 5)
    action1 shouldBe TransmitMessage
    action2 shouldBe TransmitMessage
  }

  it should "update the peer total difficulty when receiving a NewBlock" in new TestSetup {
    //given
    val newBlock = NewBlock(baseBlock, initialPeerInfo.totalDifficulty + 1)

    //when
    val MessageHandlingResult(msgHandler, action) = initialMsgHandler.receivingMessage(newBlock)

    //
    msgHandler.peerInfo shouldBe initialPeerInfo.withTotalDifficulty(newBlock.totalDifficulty)
    action shouldBe TransmitMessage
  }

  it should "update the fork accepted when receiving the fork block" in new TestSetup {

    import Fixtures.Blocks._

    //given
    val blockHeaders = BlockHeaders(Seq(DaoForkBlock.header))

    //when
    val MessageHandlingResult(msgHandler, action) = initialMsgHandler.receivingMessage(blockHeaders)

    //
    msgHandler.peerInfo shouldBe initialPeerInfo.withForkAccepted(true)
    action shouldBe TransmitMessage
  }

  it should "disconnect from a peer with different fork block" in new TestSetup {

    import Fixtures.Blocks._

    //given
    val blockHeaders = BlockHeaders(Seq(Genesis.header.copy(number = Fixtures.Blocks.DaoForkBlock.header.number)))

    //when
    val MessageHandlingResult(msgHandler, action) = initialMsgHandler.receivingMessage(blockHeaders)

    //
    msgHandler.peerInfo shouldBe initialPeerInfo
    action shouldBe TransmitMessage
    peerProbe.expectMsg(DisconnectPeer(Disconnect.Reasons.UselessPeer))
  }

  it should "stay connected to pre fork peer until reaching the fork" in new TestSetup {

    import scala.concurrent.ExecutionContext.Implicits.global

    val rlpxConnection = TestProbe()
    val nodeStatus = NodeStatus(
      key = crypto.generateKeyPair(),
      serverStatus = ServerStatus.NotListening)
    val nodeStatusHolder = Agent(nodeStatus)
    val peerActor = TestActorRef(Props(new PeerActor(
      new InetSocketAddress("127.0.0.1", 0),
      nodeStatusHolder,
      _ => rlpxConnection.ref,
      peerConf,
      storagesInstance.storages.appStateStorage,
      blockchain,
      None,
      Some(forkResolver),
      messageHandlerBuilder = (peerInfo, peer) =>
        EtcMessageHandler(peer, peerInfo, Some(forkResolver), storagesInstance.storages.appStateStorage, peerConf, blockchain)
    )))

    peerActor ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peerActor, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = blockchainConfig.daoForkBlockTotalDifficulty - 2000000, // remote is before the fork
      bestHash = ByteString("blockhash"),
      genesisHash = Fixtures.Blocks.Genesis.header.hash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peerActor, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peerActor, RLPxConnectionHandler.MessageReceived(BlockHeaders(Nil)))
    // ask for highest block
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peerActor, RLPxConnectionHandler.MessageReceived(BlockHeaders(Nil)))

    rlpxConnection.send(peerActor, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Pong) => () }

    val nonDaoHeader = Fixtures.Blocks.Genesis.header.copy(number = Fixtures.Blocks.DaoForkBlock.header.number)
    rlpxConnection.send(peerActor, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonDaoHeader))))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  trait TestSetup {
    implicit val system = ActorSystem("EtcMessageHandlerSpec_System")

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain = BlockchainImpl(storagesInstance.storages)
    blockchain.save(Fixtures.Blocks.Genesis.header)

    val blockchainConfig = BlockchainConfig(Config.config)
    val peerConf = new PeerConfiguration {
      override val fastSyncHostConfiguration: FastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = 200
        val maxBlocksBodiesPerMessage: Int = 200
        val maxReceiptsPerMessage: Int = 200
        val maxMptComponentsPerMessage: Int = 200
      }
      override val waitForStatusTimeout: FiniteDuration = 30 seconds
      override val waitForChainCheckTimeout: FiniteDuration = 15 seconds
      override val connectMaxRetries: Int = 3
      override val connectRetryDelay: FiniteDuration = 1 second
      override val disconnectPoisonPillTimeout: FiniteDuration = 5 seconds
      override val maxPeers = 10
      override val networkId: Int = 1
    }

    val forkResolver = new ForkResolver.EtcForkResolver(blockchainConfig)

    val peerStatus = Status(
      protocolVersion = Message.PV63,
      networkId = peerConf.networkId,
      totalDifficulty = BigInt(10000),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo = EtcPeerInfo(
      remoteStatus = peerStatus,
      totalDifficulty = peerStatus.totalDifficulty,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number
    )
    val peerProbe = TestProbe()
    val peer = Peer(new InetSocketAddress("127.0.0.1", 0), peerProbe.ref)

    val initialMsgHandler = EtcMessageHandler(peer, initialPeerInfo, Some(forkResolver),
      storagesInstance.storages.appStateStorage, peerConf, blockchain)

    val baseBlockHeader = Fixtures.Blocks.Block3125369.header
    val baseBlockBody = BlockBody(Nil, Nil)
    val baseBlock = Block(baseBlockHeader, baseBlockBody)
  }

}
