package io.iohk.ethereum.network.handshaker

import java.util.concurrent.atomic.AtomicReference

import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, RemoteStatus}
import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeComplete.{HandshakeFailure, HandshakeSuccess}
import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status.StatusEnc
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockHeaders.GetBlockHeadersEnc
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello.HelloEnc
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Disconnect, Hello}
import io.iohk.ethereum.network.p2p.messages.{Capability, CommonMessages, PV164, ProtocolVersions}
import io.iohk.ethereum.utils._
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.ByteStringUtils._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EtcHandshakerSpec extends AnyFlatSpec with Matchers {

  it should "correctly connect during an appropriate handshake if no fork resolver is used" in new LocalPeerPV63Setup
    with RemotePeerPV63Setup {

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

  it should "send status with total difficulty only when peer does not support PV164" in new LocalPeerPV63Setup
    with RemotePeerPV63Setup {

    val newChainWeight = ChainWeight.zero.increase(genesisBlock.header).increase(firstBlock.header)

    blockchain.save(firstBlock, Nil, newChainWeight, saveAsBestBlock = true)

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

  it should "send status with total difficulty and latest checkpoint when peer supports PV164" in new LocalPeerPV164Setup
    with RemotePeerPV164Setup {

    val newChainWeight = ChainWeight.zero.increase(genesisBlock.header).increase(firstBlock.header)

    blockchain.save(firstBlock, Nil, newChainWeight, saveAsBestBlock = true)

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
    with RemotePeerPV63Setup {

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
    with RemotePeerPV63Setup {

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

  it should "fail if a timeout happened during hello exchange" in new TestSetup {
    val handshakerAfterTimeout = initHandshakerWithoutResolver.processTimeout
    handshakerAfterTimeout.nextMessage.map(_.messageToSend) shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.TimeoutOnReceivingAMessage)
    )
  }

  it should "fail if a timeout happened during status exchange" in new RemotePeerPV63Setup {
    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterTimeout = handshakerAfterHelloOpt.get.processTimeout
    handshakerAfterTimeout.nextMessage.map(_.messageToSend) shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.TimeoutOnReceivingAMessage)
    )
  }

  it should "fail if a timeout happened during fork block exchange" in new RemotePeerPV63Setup {
    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterStatusOpt = handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg)
    val handshakerAfterTimeout = handshakerAfterStatusOpt.get.processTimeout
    handshakerAfterTimeout.nextMessage.map(_.messageToSend) shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.TimeoutOnReceivingAMessage)
    )
  }

  it should "fail if a status msg is received with invalid network id" in new LocalPeerPV63Setup
    with RemotePeerPV63Setup {
    val wrongNetworkId = localStatus.networkId + 1

    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterStatusOpt =
      handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg.copy(networkId = wrongNetworkId))
    handshakerAfterStatusOpt.get.nextMessage.map(_.messageToSend) shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.DisconnectRequested)
    )
  }

  it should "fail if a status msg is received with invalid genesisHash" in new LocalPeerPV63Setup
    with RemotePeerPV63Setup {
    val wrongGenesisHash = concatByteStrings((localStatus.genesisHash.head + 1).toByte, localStatus.genesisHash.tail)

    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterStatusOpt =
      handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg.copy(genesisHash = wrongGenesisHash))
    handshakerAfterStatusOpt.get.nextMessage.map(_.messageToSend) shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.DisconnectRequested)
    )
  }

  it should "fail if the remote peer doesn't support PV63" in new RemotePeerPV63Setup {
    val pv62Capability = Capability("eth", ProtocolVersions.PV62.toByte)
    val handshakerAfterHelloOpt =
      initHandshakerWithResolver.applyMessage(remoteHello.copy(capabilities = Seq(pv62Capability)))
    assert(handshakerAfterHelloOpt.isDefined)
    handshakerAfterHelloOpt.get.nextMessage.leftSide shouldBe Left(
      HandshakeFailure(Disconnect.Reasons.IncompatibleP2pProtocolVersion)
    )
  }

  it should "fail if a fork resolver is used and the block from the remote peer isn't accepted" in new RemotePeerPV63Setup {
    val handshakerAfterHelloOpt = initHandshakerWithResolver.applyMessage(remoteHello)
    val handshakerAfterStatusOpt = handshakerAfterHelloOpt.get.applyMessage(remoteStatusMsg)
    val handshakerAfterForkBlockOpt = handshakerAfterStatusOpt.get.applyMessage(
      BlockHeaders(Seq(genesisBlock.header.copy(number = forkBlockHeader.number)))
    )
    assert(handshakerAfterForkBlockOpt.isDefined)
    handshakerAfterForkBlockOpt.get.nextMessage.leftSide shouldBe Left(HandshakeFailure(Disconnect.Reasons.UselessPeer))
  }

  trait TestSetup extends SecureRandomBuilder with EphemBlockchainTestSetup {

    val genesisBlock = Block(
      Fixtures.Blocks.Genesis.header,
      Fixtures.Blocks.Genesis.body
    )

    val genesisWeight = ChainWeight.zero.increase(genesisBlock.header)

    val forkBlockHeader = Fixtures.Blocks.DaoForkBlock.header

    blockchain.save(genesisBlock, Nil, genesisWeight, saveAsBestBlock = true)

    val nodeStatus = NodeStatus(
      key = generateKeyPair(secureRandom),
      serverStatus = ServerStatus.NotListening,
      discoveryStatus = ServerStatus.NotListening
    )
    lazy val nodeStatusHolder = new AtomicReference(nodeStatus)

    class MockEtcHandshakerConfiguration(pv: Int = Config.Network.protocolVersion) extends EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = None
      override val nodeStatusHolder: AtomicReference[NodeStatus] = TestSetup.this.nodeStatusHolder
      override val peerConfiguration: PeerConfiguration = Config.Network.peer
      override val blockchain: Blockchain = TestSetup.this.blockchain
      override val appStateStorage: AppStateStorage = TestSetup.this.storagesInstance.storages.appStateStorage
      override val protocolVersion: Int = pv
    }

    val etcHandshakerConfigurationWithResolver = new MockEtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = Some(
        new ForkResolver.EtcForkResolver(blockchainConfig.daoForkConfig.get)
      )
    }

    val initHandshakerWithoutResolver = EtcHandshaker(new MockEtcHandshakerConfiguration(ProtocolVersions.PV164))
    val initHandshakerWithResolver = EtcHandshaker(etcHandshakerConfigurationWithResolver)

    val firstBlock =
      genesisBlock.copy(header = genesisBlock.header.copy(parentHash = genesisBlock.header.hash, number = 1))
  }

  trait LocalPeerSetup extends TestSetup {
    val localHello = Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = Config.clientId,
      capabilities = Seq(Etc164Capability, Eth63Capability),
      listenPort = 0, //Local node not listening
      nodeId = ByteString(nodeStatus.nodeId)
    )

    val localGetBlockHeadersRequest =
      GetBlockHeaders(Left(forkBlockHeader.number), maxHeaders = 1, skip = 0, reverse = false)
  }

  trait LocalPeerPV63Setup extends LocalPeerSetup {
    val localStatusMsg = CommonMessages.Status(
      protocolVersion = ProtocolVersions.PV63,
      networkId = Config.Network.peer.networkId,
      totalDifficulty = genesisBlock.header.difficulty,
      bestHash = genesisBlock.header.hash,
      genesisHash = genesisBlock.header.hash
    )
    val localStatus = RemoteStatus(localStatusMsg)
  }

  trait LocalPeerPV164Setup extends LocalPeerSetup {
    val localStatusMsg = PV164.Status(
      protocolVersion = ProtocolVersions.PV164,
      networkId = Config.Network.peer.networkId,
      chainWeight = ChainWeight.zero.increase(genesisBlock.header),
      bestHash = genesisBlock.header.hash,
      genesisHash = genesisBlock.header.hash
    )
    val localStatus = RemoteStatus(localStatusMsg)
  }

  trait RemotePeerSetup extends TestSetup {
    val remoteNodeStatus = NodeStatus(
      key = generateKeyPair(secureRandom),
      serverStatus = ServerStatus.NotListening,
      discoveryStatus = ServerStatus.NotListening
    )
    val remotePort = 8545
  }

  trait RemotePeerPV63Setup extends RemotePeerSetup {
    val remoteHello = Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = "remote-peer",
      capabilities = Seq(Eth63Capability),
      listenPort = remotePort,
      nodeId = ByteString(remoteNodeStatus.nodeId)
    )

    val remoteStatusMsg = CommonMessages.Status(
      protocolVersion = ProtocolVersions.PV63,
      networkId = Config.Network.peer.networkId,
      totalDifficulty = 0,
      bestHash = genesisBlock.header.hash,
      genesisHash = genesisBlock.header.hash
    )

    val remoteStatus = RemoteStatus(remoteStatusMsg)
  }

  trait RemotePeerPV164Setup extends RemotePeerSetup {
    val remoteHello = Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = "remote-peer",
      capabilities = Seq(Etc164Capability, Eth63Capability),
      listenPort = remotePort,
      nodeId = ByteString(remoteNodeStatus.nodeId)
    )

    val remoteStatusMsg =
      PV164.Status(
        protocolVersion = ProtocolVersions.PV164,
        networkId = Config.Network.peer.networkId,
        chainWeight = ChainWeight.zero,
        bestHash = genesisBlock.header.hash,
        genesisHash = genesisBlock.header.hash
      )
  }
}
