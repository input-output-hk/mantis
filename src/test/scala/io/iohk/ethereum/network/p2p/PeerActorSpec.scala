package io.iohk.ethereum.network.p2p

import java.net.{InetSocketAddress, URI}

import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.utils.BlockchainConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import akka.actor.{ActorSystem, PoisonPill, Props, Terminated}
import akka.agent.Agent
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum
import io.iohk.ethereum.{Fixtures, crypto}
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.HexPrefix.bytesToNibbles
import io.iohk.ethereum.network.{ForkResolver, PeerActor, PeerMessageBusActor}
import io.iohk.ethereum.network.PeerActor.{GetMaxBlockNumber, MaxBlockNumber}
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipts, _}
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.rlp.encode
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class PeerActorSpec extends FlatSpec with Matchers {

  val blockchainConfig = BlockchainConfig(Config.config)

  "PeerActor" should "create rlpx connection and send hello message" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    rlpxConnection.expectMsgPF() {
      case RLPxConnectionHandler.SendMessage(hello: Hello) => ()
    }
  }

  it should "retry failed rlpx connection" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.watch(peer)

    (0 to 3) foreach { _ =>
      time.advance(5.seconds)
      rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
      rlpxConnection.reply(RLPxConnectionHandler.ConnectionFailed)
    }

    rlpxConnection.expectMsgClass(classOf[Terminated])
  }

  it should "try to reconnect on broken rlpx connection" in new NodeStatusSetup with HandshakerSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")

    val time = new VirtualTime

    val peerMessageBus = system.actorOf(PeerMessageBusActor.props)
    var rlpxConnection = TestProbe() // var as we actually need new instances
    val peer = TestActorRef(Props(new PeerActor(_ => {
        rlpxConnection = TestProbe()
        rlpxConnection.ref
      }, peerConf, storagesInstance.storages.appStateStorage, blockchain, peerMessageBus, Some(time.scheduler),
        Some(new ForkResolver.EtcForkResolver(blockchainConfig)), handshaker)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    rlpxConnection.expectMsgPF() {
      case RLPxConnectionHandler.SendMessage(hello: Hello) => ()
    }

    rlpxConnection.ref ! PoisonPill
    peer.unwatch(rlpxConnection.ref)
    time.advance(2.seconds)
    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
  }

  it should "successfully connect to ETC peer" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = blockchainConfig.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))

    // ask for highest block
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Nil)))

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Pong()))
  }

  it should "disconnect from non-ETC peer" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val header = BlockHeader(
      ByteString("unused"), ByteString("unused"), ByteString("unused"), ByteString("unused"),
      ByteString("unused"), ByteString("unused"), ByteString("unused"),
      blockchainConfig.daoForkBlockTotalDifficulty + 100000, 3000000 ,0, 0, 0,
      ByteString("unused"),ByteString("unused"),ByteString("unused"))
    storagesInstance.storages.appStateStorage.putBestBlockNumber(3000000) // after the fork
    blockchain.save(header)
    storagesInstance.storages.blockNumberMappingStorage.put(3000000, header.hash)

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = blockchainConfig.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  it should "disconnect from non-ETC peer (when node is before fork)" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = blockchainConfig.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))

    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  it should "stay connected to pre fork peer until reaching the fork" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = blockchainConfig.daoForkBlockTotalDifficulty - 2000000, // remote is before the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Nil)))
    // ask for highest block
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Nil)))

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Pong) => () }

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  it should "disconnect on Hello timeout" in new TestSetup {
    val connection = TestProbe()

    peer ! PeerActor.HandleConnection(connection.ref, new InetSocketAddress("localhost", 9000))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.HandleConnection])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    time.advance(5.seconds)
    rlpxConnection.expectMsg(5.seconds, RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.TimeoutOnReceivingAMessage)))
  }

  it should "respond to fork block request during the handshake" in new TestSetup {
    //Save dao fork block
    blockchain.save(Fixtures.Blocks.DaoForkBlock.header)

    //Handshake till EtcForkBlockExchangeState
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = blockchainConfig.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }

    //Request dao fork block from the peer
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Left(daoForkBlockNumber), 1, 0, false)))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(Fixtures.Blocks.DaoForkBlock.header))))
  }

  it should "return Receipts for block hashes" in new TestSetup {
    //given
    val receiptsHashes = Seq(
      ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
      ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")))

    val receipts: Seq[Seq[Receipt]] = Seq(Seq(),Seq())

    blockchain.save(receiptsHashes(0), receipts(0))
    blockchain.save(receiptsHashes(1), receipts(1))

    setupConnection()

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetReceipts(receiptsHashes)))

    //then
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Receipts(receipts)))
  }

  it should "return BlockBodies for block hashes" in new TestSetup {
    //given
    val blockBodiesHashes = Seq(
      ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
      ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")))

    val blockBodies = Seq(blockBody,blockBody)

    blockchain.save(blockBodiesHashes(0), blockBodies(0))
    blockchain.save(blockBodiesHashes(1), blockBodies(1))

    setupConnection()

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetBlockBodies(blockBodiesHashes)))

    //then
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockBodies(blockBodies)))
  }

  it should "return block headers by block number" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 4)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)
    blockchain.save(etcForkBlockHeader.copy(number = 5))
    blockchain.save(etcForkBlockHeader.copy(number = 6))

    setupConnection()

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Left(3), 2, 0, reverse = false)))

    //then
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers by block number when response is shorter then what was requested" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 4)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)

    setupConnection()

      //when
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Left(3), 3, 0, reverse = false)))

      //then
      rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers by block number in reverse order" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 2)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)
    blockchain.save(etcForkBlockHeader.copy(number = 1))

    setupConnection()

      //when
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Left(3), 2, 0, reverse = true)))

      //then
      rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers by block hash" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 4)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)
    blockchain.save(etcForkBlockHeader.copy(number = 5))
    blockchain.save(etcForkBlockHeader.copy(number = 6))

    setupConnection()

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Right(firstHeader.hash), 2, 0, reverse = false)))

    //then
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers by block hash when skipping headers" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 5)

    blockchain.save(firstHeader)
    blockchain.save(etcForkBlockHeader.copy(number = 4))
    blockchain.save(secondHeader)
    blockchain.save(etcForkBlockHeader.copy(number = 6))
    blockchain.save(etcForkBlockHeader.copy(number = 7))

    setupConnection()

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Right(firstHeader.hash), maxHeaders = 2, skip = 1, reverse = false)))

    //then
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers in reverse when there are skipped blocks" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 1)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)

    setupConnection()

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Right(firstHeader.hash), 2, 1, reverse = true)))

    //then
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers in reverse when there are skipped blocks and we are asking for blocks before genesis" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 1)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)

    setupConnection()

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Right(firstHeader.hash), 3, 1, reverse = true)))

    //then
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader))))
  }

  it should "return block headers in reverse when there are skipped blocks ending at genesis" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 4)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 2)

    blockchain.save(firstHeader)
    blockchain.save(secondHeader)

    setupConnection()

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Right(firstHeader.hash), 4, 1, reverse = true)))

    //then
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader, blockchain.genesisHeader))))
  }

  it should "update max peer when receiving new block" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = daoForkBlockNumber + 4)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), 300)

    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = daoForkBlockNumber + 2)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Nil, Nil)), 300)

    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber))

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(firstBlock))
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(secondBlock))

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber + 4))
  }

  it should "update max peer when receiving block header" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = daoForkBlockNumber + 4)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = daoForkBlockNumber + 2)
    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber))

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(firstHeader, secondHeader, blockchain.genesisHeader))))

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber + 4))
  }

  it should "update max peer when receiving new block hashes" in new TestSetup {
    //given
    val firstBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), daoForkBlockNumber + 2)
    val secondBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), daoForkBlockNumber + 5)
    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber))

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(NewBlockHashes(Seq(firstBlockHash, secondBlockHash))))

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber + 5))
  }

  it should "update max peer when sending new block" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = daoForkBlockNumber + 4)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Nil, Nil)), 300)

    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = daoForkBlockNumber + 2)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Nil, Nil)), 300)

    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber))

    //when
    peer ! PeerActor.SendMessage(firstBlock)
    peer ! PeerActor.SendMessage(secondBlock)

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber + 4))
  }

  it should "update max peer when sending block header" in new TestSetup {
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = daoForkBlockNumber + 4)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = daoForkBlockNumber + 2)
    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber))

    //when
    peer ! PeerActor.SendMessage(BlockHeaders(Seq(firstHeader)))
    peer ! PeerActor.SendMessage(BlockHeaders(Seq(secondHeader)))

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber + 4))
  }

  it should "update max peer when sending new block hashes" in new TestSetup {
    //given
    val firstBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), daoForkBlockNumber + 2)
    val secondBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), daoForkBlockNumber + 5)
    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber))

    //when
    peer ! PeerActor.SendMessage(NewBlockHashes(Seq(firstBlockHash)))
    peer ! PeerActor.SendMessage(NewBlockHashes(Seq(secondBlockHash)))

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(daoForkBlockNumber + 5))
  }

  it should "return evm code for hash" in new TestSetup {
    //given
    val fakeEvmCode = ByteString(Hex.decode("ffddaaffddaaffddaaffddaaffddaa"))
    val evmCodeHash: ByteString = ByteString(ethereum.crypto.kec256(fakeEvmCode.toArray[Byte]))

    blockchain.save(evmCodeHash, fakeEvmCode)

    setupConnection()

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetNodeData(Seq(evmCodeHash))))

    //then
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(NodeData(Seq(fakeEvmCode))))
  }

  it should "return mptNode for hash" in new TestSetup {
    //given
    val exampleNibbles = ByteString(bytesToNibbles(Hex.decode("ffddaa")))
    val exampleHash = ByteString(Hex.decode("ab"*32))
    val extensionNode: MptNode = MptExtension(exampleNibbles, Left(MptHash(exampleHash)))

    blockchain.save(extensionNode)

    setupConnection()

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(GetNodeData(Seq(extensionNode.hash))))

    //then
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(NodeData(Seq(ByteString(encode(extensionNode))))))
  }

  it should "stash disconnect message until handshaked" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))
    peer ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers)

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = blockchainConfig.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))

    // ask for highest block
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Nil)))

    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.TooManyPeers)))
  }

  trait BlockUtils {

    val blockBody = new BlockBody(Seq(), Seq())

    val etcForkBlockHeader =
      BlockHeader(
        parentHash = ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("61c808d82a3ac53231750dadc13c777b59310bd9")),
        stateRoot = ByteString(Hex.decode("614d7d358b03cbdaf0343529673be20ad45809d02487f023e047efdce9da8aff")),
        transactionsRoot = ByteString(Hex.decode("d33068a7f21bff5018a00ca08a3566a06be4196dfe9e39f96e431565a619d455")),
        receiptsRoot = ByteString(Hex.decode("7bda9aa65977800376129148cbfe89d35a016dd51c95d6e6dc1e76307d315468")),
        logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
        difficulty = BigInt("62413376722602"),
        number = BigInt(1920000),
        gasLimit = BigInt(4712384),
        gasUsed = BigInt(84000),
        unixTimestamp = 1469020839L,
        extraData = ByteString(Hex.decode("e4b883e5bda9e7a59ee4bb99e9b1bc")),
        mixHash = ByteString(Hex.decode("c52daa7054babe515b17ee98540c0889cf5e1595c5dd77496997ca84a68c8da1")),
        nonce = ByteString(Hex.decode("05276a600980199d")))

    val nonEtcForkBlockHeader =
      BlockHeader(
        parentHash = ByteString("this"),
        ommersHash = ByteString("is"),
        beneficiary = ByteString("not"),
        stateRoot = ByteString("an"),
        transactionsRoot = ByteString("ETC"),
        receiptsRoot = ByteString("fork"),
        logsBloom = ByteString("block"),
        difficulty = BigInt("62413376722602"),
        number = BigInt(1920000),
        gasLimit = BigInt(4712384),
        gasUsed = BigInt(84000),
        unixTimestamp = 1469020839L,
        extraData = ByteString("unused"),
        mixHash = ByteString("unused"),
        nonce = ByteString("unused"))
  }

  trait NodeStatusSetup {
    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening)

    val nodeStatusHolder = Agent(nodeStatus)

    val storagesInstance =  new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain: Blockchain = BlockchainImpl(storagesInstance.storages)

    val testGenesisHeader = BlockHeader(
      parentHash = ByteString("0"),
      ommersHash = ByteString("0"),
      beneficiary = ByteString("0"),
      stateRoot = ByteString("0"),
      transactionsRoot = ByteString("0"),
      receiptsRoot = ByteString("0"),
      logsBloom = ByteString("0"),
      difficulty = 0,
      number = 0,
      gasLimit = 4000,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = ByteString("0"),
      mixHash = ByteString("0"),
      nonce = ByteString("0"))
    blockchain.save(testGenesisHeader)

    val daoForkBlockNumber = 1920000

    val peerConf = new PeerConfiguration {
      override val fastSyncHostConfiguration: FastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = 200
        val maxBlocksBodiesPerMessage: Int = 200
        val maxReceiptsPerMessage: Int = 200
        val maxMptComponentsPerMessage: Int = 200
      }
      override val waitForHelloTimeout: FiniteDuration = 3 seconds
      override val waitForStatusTimeout: FiniteDuration = 30 seconds
      override val waitForChainCheckTimeout: FiniteDuration = 15 seconds
      override val connectMaxRetries: Int = 3
      override val connectRetryDelay: FiniteDuration = 1 second
      override val disconnectPoisonPillTimeout: FiniteDuration = 5 seconds
      override val maxPeers = 10
      override val networkId: Int = 1
    }

  }

  trait HandshakerSetup extends NodeStatusSetup {
    val handshakerConfiguration = new EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = Some(new ForkResolver.EtcForkResolver(blockchainConfig))
      override val nodeStatusHolder: Agent[NodeStatus] = HandshakerSetup.this.nodeStatusHolder
      override val peerConfiguration: PeerConfiguration = HandshakerSetup.this.peerConf
      override val blockchain: Blockchain = HandshakerSetup.this.blockchain
      override val appStateStorage: AppStateStorage = HandshakerSetup.this.storagesInstance.storages.appStateStorage
    }

    val handshaker = EtcHandshaker(handshakerConfiguration)
  }

  trait TestSetup extends NodeStatusSetup with BlockUtils with HandshakerSetup {

    val genesisHash = ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"))

    def setupConnection(): Unit = {
      peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

      rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
      rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

      val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

      val remoteStatus = Status(
        protocolVersion = Message.PV63,
        networkId = 0,
        totalDifficulty = blockchainConfig.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
        bestHash = ByteString("blockhash"),
        genesisHash = genesisHash)

      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))

      // ask for highest block
      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Nil)))
    }

    implicit val system = ActorSystem("PeerActorSpec_System")

    val rlpxConnection = TestProbe()

    val time = new VirtualTime

    val peerMessageBus = system.actorOf(PeerMessageBusActor.props)

    val peer = TestActorRef(Props(new PeerActor(
      _ => rlpxConnection.ref,
      peerConf,
      storagesInstance.storages.appStateStorage,
      blockchain,
      peerMessageBus,
      Some(time.scheduler),
      Some(new ForkResolver.EtcForkResolver(blockchainConfig)),
      handshaker)))
  }

}
