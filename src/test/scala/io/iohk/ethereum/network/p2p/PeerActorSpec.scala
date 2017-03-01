package io.iohk.ethereum.network.p2p

import java.net.{InetSocketAddress, URI}

import akka.actor.{ActorSystem, PoisonPill, Props, Terminated}
import akka.agent.Agent
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain, BlockchainImpl}
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.network.PeerActor.{GetMaxBlockNumber, MaxBlockNumber, SendMessage}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipt, Receipts}
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.utils.{BlockchainStatus, Config, NodeStatus, ServerStatus}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class PeerActorSpec extends FlatSpec with Matchers {

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
      rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
      rlpxConnection.reply(RLPxConnectionHandler.ConnectionFailed)
    }

    rlpxConnection.expectMsgClass(classOf[Terminated])
  }

  it should "try to reconnect on broken rlpx connection" in new NodeStatusSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")

    var rlpxConnection = TestProbe() // var as we actually need new instances
    val peer = TestActorRef(Props(new PeerActor(nodeStatusHolder, _ => {
        rlpxConnection = TestProbe()
        rlpxConnection.ref
      }, blockchain)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    rlpxConnection.expectMsgPF() {
      case RLPxConnectionHandler.SendMessage(hello: Hello) => ()
    }

    rlpxConnection.ref ! PoisonPill
    peer.unwatch(rlpxConnection.ref)
    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
  }

  it should "successfully connect to ETC peer" in new TestSetup {
    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      Config.Blockchain.daoForkBlockTotalDifficulty + 10000000, // we're at some block, after the fork
      ByteString("unused"), 0)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = Config.Blockchain.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = Config.Blockchain.genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Pong()))
  }

  it should "disconnect from non-ETC peer" in new TestSetup {
    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      Config.Blockchain.daoForkBlockTotalDifficulty + 10000000, // we're at some block, after the fork
      ByteString("unused"), 0)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = Config.Blockchain.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = Config.Blockchain.genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  it should "stay connected to non-ETC peer until reaching the fork" in new TestSetup {
    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      Config.Blockchain.daoForkBlockTotalDifficulty - 10000000, // we're at some block, before the fork
      ByteString("unused"), 0)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = Config.Blockchain.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = Config.Blockchain.genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Pong) => () }

    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(nonEtcForkBlockHeader))))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.UselessPeer)))
  }

  it should "stay connected to pre fork peer until reaching the fork" in new TestSetup {
    nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
      Config.Blockchain.daoForkBlockTotalDifficulty - 10000000, // we're at some block, before the fork
      ByteString("unused"), 0)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Message.PV63,
      networkId = 0,
      totalDifficulty = Config.Blockchain.daoForkBlockTotalDifficulty - 2000000, // remote is before the fork
      bestHash = ByteString("blockhash"),
      genesisHash = Config.Blockchain.genesisHash)

    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

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

    rlpxConnection.expectMsg(5.seconds, RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.TimeoutOnReceivingAMessage)))
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
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader, Config.Blockchain.genesisBlockHeader))))
  }

  it should "update max peer when receiving new block" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 4)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Seq.empty, Seq.empty)), 300)

    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 2)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Seq.empty, Seq.empty)), 300)

    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(0))

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(firstBlock))
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(secondBlock))

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(4))
  }

  it should "update max peer when receiving block header" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 4)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 2)
    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(0))

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(firstHeader, secondHeader, Config.Blockchain.genesisBlockHeader))))

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(4))
  }

  it should "update max peer when receiving new block hashes" in new TestSetup {
    //given
    val firstBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), 2)
    val secondBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), 5)
    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(0))

    //when
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(NewBlockHashes(Seq(firstBlockHash, secondBlockHash))))

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(5))
  }

  it should "update max peer when sending new block" in new TestSetup {
    //given
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 4)
    val firstBlock = NewBlock(Block(firstHeader, BlockBody(Seq.empty, Seq.empty)), 300)

    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 2)
    val secondBlock = NewBlock(Block(secondHeader, BlockBody(Seq.empty, Seq.empty)), 300)

    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(0))

    //when
    peer ! PeerActor.SendMessage(firstBlock)
    peer ! PeerActor.SendMessage(secondBlock)

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(4))
  }

  it should "update max peer when sending block header" in new TestSetup {
    val firstHeader: BlockHeader = etcForkBlockHeader.copy(number = 4)
    val secondHeader: BlockHeader = etcForkBlockHeader.copy(number = 2)
    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(0))

    //when
    peer ! PeerActor.SendMessage(BlockHeaders(Seq(firstHeader)))
    peer ! PeerActor.SendMessage(BlockHeaders(Seq(secondHeader)))

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(4))
  }

  it should "update max peer when sending new block hashes" in new TestSetup {
    //given
    val firstBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), 2)
    val secondBlockHash: BlockHash = BlockHash(ByteString(Hex.decode("00" * 32)), 5)
    val probe = TestProbe()

    setupConnection()

    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(0))

    //when
    peer ! PeerActor.SendMessage(NewBlockHashes(Seq(firstBlockHash)))
    peer ! PeerActor.SendMessage(NewBlockHashes(Seq(secondBlockHash)))

    //then
    peer ! GetMaxBlockNumber(probe.testActor)
    probe.expectMsg(MaxBlockNumber(5))
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
      serverStatus = ServerStatus.NotListening,
      blockchainStatus = BlockchainStatus(0, ByteString("123"), 0))

    val nodeStatusHolder = Agent(nodeStatus)

    val storagesInstance =  new SharedEphemDataSources with Storages.DefaultStorages
    val blockchain: Blockchain = BlockchainImpl(storagesInstance.storages)
  }

  trait TestSetup extends NodeStatusSetup with BlockUtils {

    def setupConnection(): Unit = {
      nodeStatusHolder.send(_.copy(blockchainStatus = BlockchainStatus(
        Config.Blockchain.daoForkBlockTotalDifficulty + 10000000, // we're at some block, after the fork
        ByteString("unused"), 0)))

      peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

      rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
      rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished)

      val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Message.PV63.toByte)), 9000, ByteString("unused"))
      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Hello) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

      val remoteStatus = Status(
        protocolVersion = Message.PV63,
        networkId = 0,
        totalDifficulty = Config.Blockchain.daoForkBlockTotalDifficulty + 100000, // remote is after the fork
        bestHash = ByteString("blockhash"),
        genesisHash = Config.Blockchain.genesisHash)

      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: Status) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetBlockHeaders) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(BlockHeaders(Seq(etcForkBlockHeader))))
    }

    implicit val system = ActorSystem("PeerActorSpec_System")

    val rlpxConnection = TestProbe()

    val peer = TestActorRef(Props(new PeerActor(nodeStatusHolder, _ => rlpxConnection.ref, blockchain)))
  }

}
