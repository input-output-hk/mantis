package io.iohk.ethereum.network.handshaker

import java.util.concurrent.atomic.AtomicReference

import akka.util.ByteString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.forkid.ForkId
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeComplete.HandshakeFailure
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeComplete.HandshakeSuccess
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.Status.StatusEnc
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities._
import io.iohk.ethereum.network.p2p.messages.ETC64
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockHeaders
import io.iohk.ethereum.network.p2p.messages.ETH62.GetBlockHeaders
import io.iohk.ethereum.network.p2p.messages.ETH62.GetBlockHeaders.GetBlockHeadersEnc
import io.iohk.ethereum.network.p2p.messages.ETH64
import io.iohk.ethereum.network.p2p.messages.ProtocolVersions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello.HelloEnc
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.ByteStringUtils._
import io.iohk.ethereum.utils._

class EtcHandshakerSpec extends AnyFlatSpec with Matchers {

  it should "correctly connect during an appropriate handshake if no fork resolver is used" in new LocalPeerETH63Setup
    with RemotePeerETH63Setup {

    initHandshakerWithoutResolver.nextMessage.map(_.messageToSend) shouldBe Right(localHello: HelloEnc)
    val handshakerAfterHelloOpt = initHandshakerWithoutResolver.applyMessage(remoteHello)
    assert(handshakerAfterHelloOpt.isDefined)
    handshakerAfterHelloOpt.get.nextMessage.map(_.messageToSend) shouldBe Right(localStatusMsg: StatusEnc)
    val handshakerAfterStatusOpt = handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg)
    assert(handshakerAfterStatusOpt.isDefined)

    handshakerAfterStatusOpt.get.nextMessage match {
      case Left(
            HandshakeSuccess(
              PeerInfo(
                initialStatus,
                chainWeight,
                forkAccepted,
                currentMaxBlockNumber,
                bestBlockHash
              )
            )
          ) =>
        initialStatus shouldBe remoteStatus
        chainWeight shouldBe remoteStatus.chainWeight
        bestBlockHash shouldBe remoteStatus.bestHash
        currentMaxBlockNumber shouldBe 0
        forkAccepted shouldBe true
      case _ => fail()
    }
  }

  it should "send status with total difficulty only when peer does not support ETC64" in new LocalPeerETH63Setup
    with RemotePeerETH63Setup {

    val newChainWeight = ChainWeight.zero.increase(genesisBlock.header).increase(firstBlock.header)

    blockchainWriter.save(firstBlock, Nil, newChainWeight, saveAsBestBlock = true)

    val newLocalStatusMsg =
      localStatusMsg.copy(totalDifficulty = newChainWeight.totalDifficulty, bestHash = firstBlock.header.hash)

    initHandshakerWithoutResolver.nextMessage.map(_.messageToSend) shouldBe Right(localHello: HelloEnc)
    val handshakerAfterHelloOpt = initHandshakerWithoutResolver.applyMessage(remoteHello)
    assert(handshakerAfterHelloOpt.isDefined)
    handshakerAfterHelloOpt.get.nextMessage.map(_.messageToSend.underlyingMsg) shouldBe Right(newLocalStatusMsg)

    val handshakerAfterStatusOpt = handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg)
    assert(handshakerAfterStatusOpt.isDefined)
    handshakerAfterStatusOpt.get.nextMessage match {
      case Left(HandshakeSuccess(peerInfo)) =>
        peerInfo.remoteStatus.protocolVersion shouldBe localStatus.protocolVersion

      case other =>
        fail(s"Invalid handshaker state: $other")
    }
  }

  it should "send status with total difficulty and latest checkpoint when peer supports ETC64" in new LocalPeerETC64Setup
    with RemotePeerETC64Setup {

    val newChainWeight = ChainWeight.zero.increase(genesisBlock.header).increase(firstBlock.header)

    blockchainWriter.save(firstBlock, Nil, newChainWeight, saveAsBestBlock = true)

    val newLocalStatusMsg =
      localStatusMsg
        .copy(
          chainWeight = newChainWeight,
          bestHash = firstBlock.header.hash
        )

    initHandshakerWithoutResolver.nextMessage.map(_.messageToSend) shouldBe Right(localHello: HelloEnc)

    val handshakerAfterHelloOpt = initHandshakerWithoutResolver.applyMessage(remoteHello)
    assert(handshakerAfterHelloOpt.isDefined)
    handshakerAfterHelloOpt.get.nextMessage.map(_.messageToSend.underlyingMsg) shouldBe Right(newLocalStatusMsg)

    val handshakerAfterStatusOpt = handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg)
    assert(handshakerAfterStatusOpt.isDefined)
    handshakerAfterStatusOpt.get.nextMessage match {
      case Left(HandshakeSuccess(peerInfo)) =>
        peerInfo.remoteStatus.protocolVersion shouldBe localStatus.protocolVersion

      case other =>
        fail(s"Invalid handshaker state: $other")
    }
  }

  it should "correctly connect during an appropriate handshake if a fork resolver is used and the remote peer has the DAO block" in new LocalPeerSetup
    with RemotePeerETH63Setup {

    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterStatusOpt = handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg)
    assert(handshakerAfterStatusOpt.isDefined)
    handshakerAfterStatusOpt.get.nextMessage.map(_.messageToSend) shouldBe Right(
      localGetBlockHeadersRequest: GetBlockHeadersEnc
    )
    val handshakerAfterForkOpt = handshakerAfterStatusOpt.get.applyMessage(BlockHeaders(Seq(forkBlockHeader)))
    assert(handshakerAfterForkOpt.isDefined)

    handshakerAfterForkOpt.get.nextMessage match {
      case Left(
            HandshakeSuccess(
              PeerInfo(
                initialStatus,
                chainWeight,
                forkAccepted,
                currentMaxBlockNumber,
                bestBlockHash
              )
            )
          ) =>
        initialStatus shouldBe remoteStatus
        chainWeight shouldBe remoteStatus.chainWeight
        bestBlockHash shouldBe remoteStatus.bestHash
        currentMaxBlockNumber shouldBe 0
        forkAccepted shouldBe true
      case _ => fail()
    }
  }

  it should "correctly connect during an appropriate handshake if a fork resolver is used and the remote peer doesn't have the DAO block" in new LocalPeerSetup
    with RemotePeerETH63Setup {

    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterStatusOpt = handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg)
    assert(handshakerAfterStatusOpt.isDefined)
    handshakerAfterStatusOpt.get.nextMessage.map(_.messageToSend) shouldBe Right(
      localGetBlockHeadersRequest: GetBlockHeadersEnc
    )
    val handshakerAfterFork = handshakerAfterStatusOpt.get.applyMessage(BlockHeaders(Nil))
    assert(handshakerAfterStatusOpt.isDefined)

    handshakerAfterFork.get.nextMessage match {
      case Left(
            HandshakeSuccess(
              PeerInfo(
                initialStatus,
                chainWeight,
                forkAccepted,
                currentMaxBlockNumber,
                bestBlockHash
              )
            )
          ) =>
        initialStatus shouldBe remoteStatus
        chainWeight shouldBe remoteStatus.chainWeight
        bestBlockHash shouldBe remoteStatus.bestHash
        currentMaxBlockNumber shouldBe 0
        forkAccepted shouldBe false
      case _ => fail()
    }
  }

  it should "send status with fork id when peer supports ETH64" in new LocalPeerETH64Setup
    with RemotePeerETH64Setup {

    val newChainWeight = ChainWeight.zero.increase(genesisBlock.header).increase(firstBlock.header)

    blockchain.save(firstBlock, Nil, newChainWeight, saveAsBestBlock = true)

    val newLocalStatusMsg =
      localStatusMsg
        .copy(
          bestHash = firstBlock.header.hash,
          totalDifficulty = newChainWeight.totalDifficulty,
          forkId = ForkId(0xfc64ec04L, Some(1150000))
        )

    initHandshakerWithoutResolver.nextMessage.map(_.messageToSend) shouldBe Right(localHello: HelloEnc)

    val handshakerAfterHelloOpt = initHandshakerWithoutResolver.applyMessage(remoteHello)
    assert(handshakerAfterHelloOpt.isDefined)

    handshakerAfterHelloOpt.get.nextMessage.map(_.messageToSend.underlyingMsg) shouldBe Right(newLocalStatusMsg)

    val handshakerAfterStatusOpt = handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg)
    assert(handshakerAfterStatusOpt.isDefined)

    handshakerAfterStatusOpt.get.nextMessage match {
      case Left(HandshakeSuccess(peerInfo)) =>
        peerInfo.remoteStatus.protocolVersion shouldBe localStatus.protocolVersion

      case other =>
        fail(s"Invalid handshaker state: $other")
    }
  }

  it should "fail if a timeout happened during hello exchange" in new TestSetup {
    val handshakerAfterTimeout = initHandshakerWithoutResolver.processTimeout
    handshakerAfterTimeout.nextMessage.map(_.messageToSend) shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.TimeoutOnReceivingAMessage)
    )
  }

  it should "fail if a timeout happened during status exchange" in new RemotePeerETH63Setup {
    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterTimeout = handshakerAfterHelloOpt.get.processTimeout
    handshakerAfterTimeout.nextMessage.map(_.messageToSend) shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.TimeoutOnReceivingAMessage)
    )
  }

  it should "fail if a timeout happened during fork block exchange" in new RemotePeerETH63Setup {
    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterStatusOpt = handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg)
    val handshakerAfterTimeout = handshakerAfterStatusOpt.get.processTimeout
    handshakerAfterTimeout.nextMessage.map(_.messageToSend) shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.TimeoutOnReceivingAMessage)
    )
  }

  it should "fail if a status msg is received with invalid network id" in new LocalPeerETH63Setup
    with RemotePeerETH63Setup {
    val wrongNetworkId = localStatus.networkId + 1

    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterStatusOpt =
      handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg.copy(networkId = wrongNetworkId))
    handshakerAfterStatusOpt.get.nextMessage.map(_.messageToSend) shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.DisconnectRequested)
    )
  }

  it should "fail if a status msg is received with invalid genesisHash" in new LocalPeerETH63Setup
    with RemotePeerETH63Setup {
    val wrongGenesisHash = concatByteStrings((localStatus.genesisHash.head + 1).toByte, localStatus.genesisHash.tail)

    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterStatusOpt =
      handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg.copy(genesisHash = wrongGenesisHash))
    handshakerAfterStatusOpt.get.nextMessage.map(_.messageToSend) shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.DisconnectRequested)
    )
  }

  it should "fail if the remote peer doesn't support ETH63/ETC64" in new RemotePeerETH63Setup {
    val eth62Capability = ProtocolVersions.ETH62
    val handshakerAfterHelloOpt =
      initHandshakerWithResolver.applyMessage(remoteHello.copy(capabilities = Seq(eth62Capability)))
    assert(handshakerAfterHelloOpt.isDefined)
    handshakerAfterHelloOpt.get.nextMessage.leftSide shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.IncompatibleP2pProtocolVersion)
    )
  }

  it should "fail if a fork resolver is used and the block from the remote peer isn't accepted" in new RemotePeerETH63Setup {
    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterStatusOpt = handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg)
    val handshakerAfterForkBlockOpt = handshakerAfterStatusOpt.get.applyMessage(
      BlockHeaders(Seq(genesisBlock.header.copy(number = forkBlockHeader.number)))
    )
    assert(handshakerAfterForkBlockOpt.isDefined)
    handshakerAfterForkBlockOpt.get.nextMessage.leftSide shouldBe Left(HandshakeFailure(Disconnect.Reasons.UselessPeer))
  }

  trait TestSetup extends SecureRandomBuilder with EphemBlockchainTestSetup {

    val genesisBlock: Block = Block(
      Fixtures.Blocks.Genesis.header,
      Fixtures.Blocks.Genesis.body
    )

    val genesisWeight: ChainWeight = ChainWeight.zero.increase(genesisBlock.header)

    val forkBlockHeader = Fixtures.Blocks.DaoForkBlock.header

    blockchainWriter.save(genesisBlock, Nil, genesisWeight, saveAsBestBlock = true)

    val nodeStatus: NodeStatus = NodeStatus(
      key = generateKeyPair(secureRandom),
      serverStatus = ServerStatus.NotListening,
      discoveryStatus = ServerStatus.NotListening
    )
    lazy val nodeStatusHolder = new AtomicReference(nodeStatus)

    class MockEtcHandshakerConfiguration(pv: List[Capability] = blockchainConfig.capabilities)
        extends EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = None
      override val nodeStatusHolder: AtomicReference[NodeStatus] = TestSetup.this.nodeStatusHolder
      override val peerConfiguration: PeerConfiguration = Config.Network.peer
      override val blockchain: Blockchain = TestSetup.this.blockchain
      override val appStateStorage: AppStateStorage = TestSetup.this.storagesInstance.storages.appStateStorage
      override val capabilities: List[Capability] = pv
      override val blockchainReader: BlockchainReader = TestSetup.this.blockchainReader
      override val blockchainConfig: BlockchainConfig = TestSetup.this.blockchainConfig
    }

    val etcHandshakerConfigurationWithResolver: MockEtcHandshakerConfiguration = new MockEtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = Some(
        new ForkResolver.EtcForkResolver(blockchainConfig.daoForkConfig.get)
      )
    }

    val initHandshakerWithoutResolver: EtcHandshaker = EtcHandshaker(
      new MockEtcHandshakerConfiguration(List(ProtocolVersions.ETC64, ProtocolVersions.ETH63, ProtocolVersions.ETH64))
    )

    val initHandshakerWithResolver: EtcHandshaker = EtcHandshaker(etcHandshakerConfigurationWithResolver)

    val firstBlock: Block =
      genesisBlock.copy(header = genesisBlock.header.copy(parentHash = genesisBlock.header.hash, number = 1))
  }

  trait LocalPeerSetup extends TestSetup {
    val localHello: Hello = Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = Config.clientId,
      capabilities = Seq(Etc64Capability, Eth63Capability, Eth64Capability),
      listenPort = 0, //Local node not listening
      nodeId = ByteString(nodeStatus.nodeId)
    )

    val localGetBlockHeadersRequest: GetBlockHeaders =
      GetBlockHeaders(Left(forkBlockHeader.number), maxHeaders = 1, skip = 0, reverse = false)
  }

  trait LocalPeerETH63Setup extends LocalPeerSetup {
    val localStatusMsg: BaseETH6XMessages.Status = BaseETH6XMessages.Status(
      protocolVersion = ProtocolVersions.ETH63.version,
      networkId = Config.Network.peer.networkId,
      totalDifficulty = genesisBlock.header.difficulty,
      bestHash = genesisBlock.header.hash,
      genesisHash = genesisBlock.header.hash
    )
    val localStatus: RemoteStatus = RemoteStatus(localStatusMsg)
  }

  trait LocalPeerETH64Setup extends LocalPeerSetup {
    val localStatusMsg = ETH64.Status(
      protocolVersion = ProtocolVersions.ETH64.version,
      networkId = Config.Network.peer.networkId,
      totalDifficulty = genesisBlock.header.difficulty,
      bestHash = genesisBlock.header.hash,
      genesisHash = genesisBlock.header.hash,
      forkId = ForkId(1L, None)
    )
    val localStatus = RemoteStatus(localStatusMsg)
  }

  trait LocalPeerETC64Setup extends LocalPeerSetup {
    val localStatusMsg: ETC64.Status = ETC64.Status(
      protocolVersion = ProtocolVersions.ETC64.version,
      networkId = Config.Network.peer.networkId,
      chainWeight = ChainWeight.zero.increase(genesisBlock.header),
      bestHash = genesisBlock.header.hash,
      genesisHash = genesisBlock.header.hash
    )
    val localStatus: RemoteStatus = RemoteStatus(localStatusMsg)
  }

  trait RemotePeerSetup extends TestSetup {
    val remoteNodeStatus: NodeStatus = NodeStatus(
      key = generateKeyPair(secureRandom),
      serverStatus = ServerStatus.NotListening,
      discoveryStatus = ServerStatus.NotListening
    )
    val remotePort = 8545
  }

  trait RemotePeerETH63Setup extends RemotePeerSetup {
    val remoteHello: Hello = Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = "remote-peer",
      capabilities = Seq(Eth63Capability),
      listenPort = remotePort,
      nodeId = ByteString(remoteNodeStatus.nodeId)
    )

    val remoteStatusMsg: BaseETH6XMessages.Status = BaseETH6XMessages.Status(
      protocolVersion = ProtocolVersions.ETH63.version,
      networkId = Config.Network.peer.networkId,
      totalDifficulty = 0,
      bestHash = genesisBlock.header.hash,
      genesisHash = genesisBlock.header.hash
    )

    val remoteStatus: RemoteStatus = RemoteStatus(remoteStatusMsg)
  }

  trait RemotePeerETC64Setup extends RemotePeerSetup {
    val remoteHello: Hello = Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = "remote-peer",
      capabilities = Seq(Etc64Capability, Eth63Capability),
      listenPort = remotePort,
      nodeId = ByteString(remoteNodeStatus.nodeId)
    )

    val remoteStatusMsg: ETC64.Status =
      ETC64.Status(
        protocolVersion = ProtocolVersions.ETC64.version,
        networkId = Config.Network.peer.networkId,
        chainWeight = ChainWeight.zero,
        bestHash = genesisBlock.header.hash,
        genesisHash = genesisBlock.header.hash
      )
  }

  trait RemotePeerETH64Setup extends RemotePeerSetup {
    val remoteHello = Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = "remote-peer",
      capabilities = Seq(Eth64Capability),
      listenPort = remotePort,
      nodeId = ByteString(remoteNodeStatus.nodeId)
    )

    val remoteStatusMsg = ETH64.Status(
      protocolVersion = ProtocolVersions.ETH64.version,
      networkId = Config.Network.peer.networkId,
      totalDifficulty = 0,
      bestHash = genesisBlock.header.hash,
      genesisHash = genesisBlock.header.hash,
      forkId = ForkId(2L, Some(3L))
    )

    val remoteStatus = RemoteStatus(remoteStatusMsg)
  }
}
