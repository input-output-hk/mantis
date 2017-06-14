package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.network.PeerActor.{DisconnectPeer, SendMessage}
import io.iohk.ethereum.network.EtcPeerManagerActor._
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent._
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier._
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeResult
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders, NewBlockHashes}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect

/**
  * EtcPeerManager actor is in charge of keeping updated information about each peer, while also being able to
  * query it for this information.
  * In order to do so it receives events for peer creation, disconnection and new messages being sent and
  * received by each peer.
  */
class EtcPeerManagerActor(peerManagerActor: ActorRef, peerEventBusActor: ActorRef, appStateStorage: AppStateStorage,
                          forkResolverOpt: Option[ForkResolver]) extends Actor with ActorLogging {

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

  /**
    * Processes both messages for sending messages and for requesting peer information
    *
    * @param peersWithInfo, which has the peer and peer information for each handshaked peer (identified by it's id)
    */
  private def handleCommonMessages(peersWithInfo: PeersWithInfo): Receive = {
    case GetHandshakedPeers =>
      sender() ! HandshakedPeers(peersWithInfo.map{ case (_, PeerWithInfo(peer, peerInfo)) => peer -> peerInfo })

    case PeerInfoRequest(peerId) =>
      val peerInfoOpt = peersWithInfo.get(peerId).map{case PeerWithInfo(_, peerInfo) => peerInfo}
      sender() ! PeerInfoResponse(peerInfoOpt)

    case EtcPeerManagerActor.SendMessage(message, peerId) =>
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
      context become handleMessages(newPeersWithInfo)

    case PeerHandshakeSuccessful(peer, peerInfo: PeerInfo) =>
      peerEventBusActor ! Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id)))
      peerEventBusActor ! Subscribe(MessageClassifier(msgCodesWithInfo, PeerSelector.WithId(peer.id)))

      //Ask for the highest block from the peer
      peer.ref ! SendMessage(GetBlockHeaders(Right(peerInfo.remoteStatus.bestHash), 1, 0, false))
      context become handleMessages(peersWithInfo + (peer.id -> PeerWithInfo(peer, peerInfo)))

    case PeerDisconnected(peerId) if peersWithInfo.contains(peerId) =>
      peerEventBusActor ! Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peerId)))
      peerEventBusActor ! Unsubscribe(MessageClassifier(msgCodesWithInfo, PeerSelector.WithId(peerId)))
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
  private def updatePeersWithInfo(peers: PeersWithInfo, peerId: PeerId, message: Message,
                                  messageHandler: (Message, PeerWithInfo) => PeerInfo): PeersWithInfo = {
    if(peers.contains(peerId)){
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
    updateMaxBlock(message)(initialPeerWithInfo.peerInfo)

  /**
    * Processes the message and the old peer info and returns the peer info
    *
    * @param message to be processed
    * @param initialPeerWithInfo from before the message was processed
    * @return new updated peer info
    */
  private def handleReceivedMessage(message: Message, initialPeerWithInfo: PeerWithInfo): PeerInfo =
    (updateTotalDifficulty(message) _
      andThen updateForkAccepted(message, initialPeerWithInfo.peer)
      andThen updateMaxBlock(message)
      )(initialPeerWithInfo.peerInfo)


  /**
    * Processes the message and updates the total difficulty of the peer
    *
    * @param message to be processed
    * @param initialPeerInfo from before the message was processed
    * @return new peer info with the total difficulty updated
    */
  private def updateTotalDifficulty(message: Message)(initialPeerInfo: PeerInfo): PeerInfo = message match {
    case newBlock: NewBlock =>
      initialPeerInfo.withTotalDifficulty(newBlock.totalDifficulty)
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
          log.info("Received fork block header with fork: {}", newFork)

          if (!forkResolver.isAccepted(newFork)) {
            log.warning("Peer is not running the accepted fork, disconnecting")
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
    def update(ns: Seq[BigInt]): PeerInfo = {
      val maxBlockNumber = ns.fold(0: BigInt) { case (a, b) => if (a > b) a else b }
      if (maxBlockNumber > appStateStorage.getEstimatedHighestBlock())
        appStateStorage.putEstimatedHighestBlock(maxBlockNumber)

      if (maxBlockNumber > initialPeerInfo.maxBlockNumber)
        initialPeerInfo.withMaxBlockNumber(maxBlockNumber)
      else
        initialPeerInfo
    }

    message match {
      case m: BlockHeaders =>
        update(m.headers.map(_.number))
      case m: NewBlock =>
        update(Seq(m.block.header.number))
      case m: NewBlockHashes =>
        update(m.hashes.map(_.number))
      case _ => initialPeerInfo
    }
  }

}

object EtcPeerManagerActor {

  val msgCodesWithInfo: Set[Int] = Set(BlockHeaders.code, NewBlock.code, NewBlockHashes.code)

  case class PeerInfo(remoteStatus: Status,
                      totalDifficulty: BigInt,
                      forkAccepted: Boolean,
                      maxBlockNumber: BigInt) extends HandshakeResult {

    def withTotalDifficulty(totalDifficulty: BigInt): PeerInfo = copy(totalDifficulty = totalDifficulty)

    def withForkAccepted(forkAccepted: Boolean): PeerInfo = copy(forkAccepted = forkAccepted)

    def withMaxBlockNumber(maxBlockNumber: BigInt): PeerInfo = copy(maxBlockNumber = maxBlockNumber)

  }

  private case class PeerWithInfo(peer: Peer, peerInfo: PeerInfo)

  case object GetHandshakedPeers

  case class HandshakedPeers(peers: Map[Peer, PeerInfo])

  case class PeerInfoRequest(peerId: PeerId)

  case class PeerInfoResponse(peerInfo: Option[PeerInfo])

  case class SendMessage(message: MessageSerializable, peerId: PeerId)

  def props(peerManagerActor: ActorRef, peerEventBusActor: ActorRef,
            appStateStorage: AppStateStorage, forkResolverOpt: Option[ForkResolver]): Props =
    Props(new EtcPeerManagerActor(peerManagerActor, peerEventBusActor, appStateStorage, forkResolverOpt))

}
