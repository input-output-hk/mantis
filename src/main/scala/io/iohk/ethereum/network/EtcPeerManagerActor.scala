package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.network.EtcPeerManagerActor._
import io.iohk.ethereum.network.PeerActor.{DisconnectPeer, SendMessage}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent._
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier._
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeResult
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders, NewBlockHashes}
import io.iohk.ethereum.network.p2p.messages.PV164.NewBlock
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.network.p2p.messages.{Codes, CommonMessages, PV164}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}
import io.iohk.ethereum.utils.ByteStringUtils

/**
  * EtcPeerManager actor is in charge of keeping updated information about each peer, while also being able to
  * query it for this information.
  * In order to do so it receives events for peer creation, disconnection and new messages being sent and
  * received by each peer.
  */
class EtcPeerManagerActor(
    peerManagerActor: ActorRef,
    peerEventBusActor: ActorRef,
    appStateStorage: AppStateStorage,
    forkResolverOpt: Option[ForkResolver]
) extends Actor
    with ActorLogging {

  private type PeersWithInfo = Map[PeerId, PeerWithInfo]

  //Subscribe to the event of any peer getting handshaked
  peerEventBusActor ! Subscribe(PeerHandshaked)

  override def receive: Receive = handleMessages(Map.empty)

  /**
    * Processes both messages for updating the information about each peer and for requesting this information
    *
    * @param peersWithInfo, which has the peer and peer information for each handshaked peer (identified by it's id)
    */
  def handleMessages(peersWithInfo: PeersWithInfo): Receive =
    handleCommonMessages(peersWithInfo) orElse handlePeersInfoEvents(peersWithInfo)

  private def peerHasUpdatedBestBlock(peerInfo: PeerInfo): Boolean = {
    val peerBestBlockIsItsGenesisBlock = peerInfo.bestBlockHash == peerInfo.remoteStatus.genesisHash
    peerBestBlockIsItsGenesisBlock || (!peerBestBlockIsItsGenesisBlock && peerInfo.maxBlockNumber > 0)
  }

  /**
    * Processes both messages for sending messages and for requesting peer information
    *
    * @param peersWithInfo, which has the peer and peer information for each handshaked peer (identified by it's id)
    */
  private def handleCommonMessages(peersWithInfo: PeersWithInfo): Receive = {
    case GetHandshakedPeers =>
      // Provide only peers which already responded to request for best block hash, and theirs best block hash is different
      // form their genesis block
      sender() ! HandshakedPeers(peersWithInfo.collect {
        case (_, PeerWithInfo(peer, peerInfo)) if peerHasUpdatedBestBlock(peerInfo) => peer -> peerInfo
      })

    case PeerInfoRequest(peerId) =>
      val peerInfoOpt = peersWithInfo.get(peerId).map { case PeerWithInfo(_, peerInfo) => peerInfo }
      sender() ! PeerInfoResponse(peerInfoOpt)

    case EtcPeerManagerActor.SendMessage(message, peerId) =>
      NetworkMetrics.SentMessagesCounter.increment()
      val newPeersWithInfo = updatePeersWithInfo(peersWithInfo, peerId, message.underlyingMsg, handleSentMessage)
      peerManagerActor ! PeerManagerActor.SendMessage(message, peerId)
      context become handleMessages(newPeersWithInfo)
  }

  /**
    * Processes events and updating the information about each peer
    *
    * @param peersWithInfo, which has the peer and peer information for each handshaked peer (identified by it's id)
    */
  private def handlePeersInfoEvents(peersWithInfo: PeersWithInfo): Receive = {

    case MessageFromPeer(message, peerId) if peersWithInfo.contains(peerId) =>
      val newPeersWithInfo = updatePeersWithInfo(peersWithInfo, peerId, message, handleReceivedMessage)
      NetworkMetrics.ReceivedMessagesCounter.increment()
      context become handleMessages(newPeersWithInfo)

    case PeerHandshakeSuccessful(peer, peerInfo: PeerInfo) =>
      peerEventBusActor ! Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id)))
      peerEventBusActor ! Subscribe(MessageClassifier(msgCodesWithInfo, PeerSelector.WithId(peer.id)))

      //Ask for the highest block from the peer
      peer.ref ! SendMessage(GetBlockHeaders(Right(peerInfo.remoteStatus.bestHash), 1, 0, false))
      NetworkMetrics.registerAddHandshakedPeer(peer)
      context become handleMessages(peersWithInfo + (peer.id -> PeerWithInfo(peer, peerInfo)))

    case PeerDisconnected(peerId) if peersWithInfo.contains(peerId) =>
      peerEventBusActor ! Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peerId)))
      peerEventBusActor ! Unsubscribe(MessageClassifier(msgCodesWithInfo, PeerSelector.WithId(peerId)))
      NetworkMetrics.registerRemoveHandshakedPeer(peersWithInfo(peerId).peer)
      context become handleMessages(peersWithInfo - peerId)

  }

  /**
    * Processes the message, updating the information for each peer
    *
    * @param peers with the information for each peer
    * @param peerId from whom the message was received (or who sent the message)
    * @param message to be processed
    * @param messageHandler for processing the message and obtaining the new peerInfo
    * @return new information for each peer
    */
  private def updatePeersWithInfo(
      peers: PeersWithInfo,
      peerId: PeerId,
      message: Message,
      messageHandler: (Message, PeerWithInfo) => PeerInfo
  ): PeersWithInfo = {
    if (peers.contains(peerId)) {
      val peerWithInfo = peers(peerId)
      val newPeerInfo = messageHandler(message, peerWithInfo)
      peers + (peerId -> peerWithInfo.copy(peerInfo = newPeerInfo))
    } else
      peers
  }

  /**
    * Processes the message and the old peer info and returns the peer info
    *
    * @param message to be processed
    * @param initialPeerWithInfo from before the message was processed
    * @return new updated peer info
    */
  private def handleSentMessage(message: Message, initialPeerWithInfo: PeerWithInfo): PeerInfo =
    initialPeerWithInfo.peerInfo

  /**
    * Processes the message and the old peer info and returns the peer info
    *
    * @param message to be processed
    * @param initialPeerWithInfo from before the message was processed
    * @return new updated peer info
    */
  private def handleReceivedMessage(message: Message, initialPeerWithInfo: PeerWithInfo): PeerInfo = {
    (updateChainWeight(message) _
      andThen updateForkAccepted(message, initialPeerWithInfo.peer)
      andThen updateMaxBlock(message))(initialPeerWithInfo.peerInfo)
  }

  /**
    * Processes the message and updates the chain weight of the peer
    *
    * @param message to be processed
    * @param initialPeerInfo from before the message was processed
    * @return new peer info with the total difficulty updated
    */
  private def updateChainWeight(message: Message)(initialPeerInfo: PeerInfo): PeerInfo =
    message match {
      case newBlock: CommonMessages.NewBlock =>
        initialPeerInfo.copy(chainWeight = ChainWeight.totalDifficultyOnly(newBlock.totalDifficulty))
      case newBlock: PV164.NewBlock => initialPeerInfo.copy(chainWeight = newBlock.chainWeight)
      case _ => initialPeerInfo
    }

  /**
    * Processes the message and updates if the fork block was accepted from the peer
    *
    * @param message to be processed
    * @param initialPeerInfo from before the message was processed
    * @return new peer info with the fork block accepted value updated
    */
  private def updateForkAccepted(message: Message, peer: Peer)(initialPeerInfo: PeerInfo): PeerInfo = message match {
    case BlockHeaders(blockHeaders) =>
      val newPeerInfoOpt: Option[PeerInfo] =
        for {
          forkResolver <- forkResolverOpt
          forkBlockHeader <- blockHeaders.find(_.number == forkResolver.forkBlockNumber)
        } yield {
          val newFork = forkResolver.recognizeFork(forkBlockHeader)
          log.debug("Received fork block header with fork: {}", newFork)

          if (!forkResolver.isAccepted(newFork)) {
            log.debug("Peer is not running the accepted fork, disconnecting")
            peer.ref ! DisconnectPeer(Disconnect.Reasons.UselessPeer)
            initialPeerInfo
          } else
            initialPeerInfo.withForkAccepted(true)
        }
      newPeerInfoOpt.getOrElse(initialPeerInfo)

    case _ => initialPeerInfo
  }

  /**
    * Processes the message and updates the max block number from the peer
    *
    * @param message to be processed
    * @param initialPeerInfo from before the message was processed
    * @return new peer info with the max block number updated
    */
  private def updateMaxBlock(message: Message)(initialPeerInfo: PeerInfo): PeerInfo = {
    def update(ns: Seq[(BigInt, ByteString)]): PeerInfo = {
      if (ns.isEmpty) {
        initialPeerInfo
      } else {
        val (maxBlockNumber, maxBlockHash) = ns.maxBy(_._1)
        if (maxBlockNumber > appStateStorage.getEstimatedHighestBlock())
          appStateStorage.putEstimatedHighestBlock(maxBlockNumber)

        if (maxBlockNumber > initialPeerInfo.maxBlockNumber) {
          initialPeerInfo.withBestBlockData(maxBlockNumber, maxBlockHash)
        } else
          initialPeerInfo
      }
    }

    message match {
      case m: BlockHeaders =>
        update(m.headers.map(header => (header.number, header.hash)))
      case m: CommonMessages.NewBlock =>
        update(Seq((m.block.header.number, m.block.header.hash)))
      case m: NewBlock =>
        update(Seq((m.block.header.number, m.block.header.hash)))
      case m: NewBlockHashes =>
        update(m.hashes.map(h => (h.number, h.hash)))
      case _ => initialPeerInfo
    }
  }

}

object EtcPeerManagerActor {

  val msgCodesWithInfo: Set[Int] = Set(Codes.BlockHeadersCode, Codes.NewBlockCode, Codes.NewBlockHashesCode)

  /**
    * RemoteStatus was created to decouple status information from protocol status messages
    * (they are different versions of Status msg)
    */
  case class RemoteStatus(
      protocolVersion: Int,
      networkId: Int,
      chainWeight: ChainWeight,
      bestHash: ByteString,
      genesisHash: ByteString
  ) {
    override def toString: String = {
      s"RemoteStatus { " +
        s"protocolVersion: $protocolVersion, " +
        s"networkId: $networkId, " +
        s"chainWeight: $chainWeight, " +
        s"bestHash: ${ByteStringUtils.hash2string(bestHash)}, " +
        s"genesisHash: ${ByteStringUtils.hash2string(genesisHash)}," +
        s"}"
    }
  }

  object RemoteStatus {
    def apply(status: PV164.Status): RemoteStatus = {
      RemoteStatus(status.protocolVersion, status.networkId, status.chainWeight, status.bestHash, status.genesisHash)
    }

    def apply(status: CommonMessages.Status): RemoteStatus = {
      RemoteStatus(
        status.protocolVersion,
        status.networkId,
        ChainWeight.totalDifficultyOnly(status.totalDifficulty),
        status.bestHash,
        status.genesisHash
      )
    }
  }

  case class PeerInfo(
      remoteStatus: RemoteStatus, // Updated only after handshaking
      chainWeight: ChainWeight,
      forkAccepted: Boolean,
      maxBlockNumber: BigInt,
      bestBlockHash: ByteString
  ) extends HandshakeResult {

    def withForkAccepted(forkAccepted: Boolean): PeerInfo = copy(forkAccepted = forkAccepted)

    def withBestBlockData(maxBlockNumber: BigInt, bestBlockHash: ByteString): PeerInfo =
      copy(maxBlockNumber = maxBlockNumber, bestBlockHash = bestBlockHash)

    def withChainWeight(weight: ChainWeight): PeerInfo =
      copy(chainWeight = weight)

    override def toString: String =
      s"PeerInfo {" +
        s" chainWeight: $chainWeight," +
        s" forkAccepted: $forkAccepted," +
        s" maxBlockNumber: $maxBlockNumber," +
        s" bestBlockHash: ${ByteStringUtils.hash2string(bestBlockHash)}," +
        s" handshakeStatus: $remoteStatus" +
        s" }"
  }

  object PeerInfo {
    def apply(remoteStatus: RemoteStatus, forkAccepted: Boolean): PeerInfo = {
      PeerInfo(
        remoteStatus,
        remoteStatus.chainWeight,
        forkAccepted,
        0,
        remoteStatus.bestHash
      )
    }

    def withForkAccepted(remoteStatus: RemoteStatus): PeerInfo =
      PeerInfo(remoteStatus, forkAccepted = true)

    def withNotForkAccepted(remoteStatus: RemoteStatus): PeerInfo =
      PeerInfo(remoteStatus, forkAccepted = false)
  }

  private case class PeerWithInfo(peer: Peer, peerInfo: PeerInfo)

  case object GetHandshakedPeers

  case class HandshakedPeers(peers: Map[Peer, PeerInfo])

  case class PeerInfoRequest(peerId: PeerId)

  case class PeerInfoResponse(peerInfo: Option[PeerInfo])

  case class SendMessage(message: MessageSerializable, peerId: PeerId)

  def props(
      peerManagerActor: ActorRef,
      peerEventBusActor: ActorRef,
      appStateStorage: AppStateStorage,
      forkResolverOpt: Option[ForkResolver]
  ): Props =
    Props(new EtcPeerManagerActor(peerManagerActor, peerEventBusActor, appStateStorage, forkResolverOpt))

}
