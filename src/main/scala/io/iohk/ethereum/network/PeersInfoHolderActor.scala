package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.network.PeerActor.DisconnectPeer
import io.iohk.ethereum.network.PeersInfoHolderActor._
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent._
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageReceivedClassifier, PeerDisconnectedClassifier, PeerHandshaked}
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeResult
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, NewBlockHashes}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect

class PeersInfoHolderActor(peerEventBusActor: ActorRef, appStateStorage: AppStateStorage,
                           forkResolverOpt: Option[ForkResolver]) extends Actor with ActorLogging {

  val msgsCodesWithPeerInfo: Set[Int] = Set(BlockHeaders.code, NewBlock.code, NewBlockHashes.code)

  peerEventBusActor ! Subscribe(PeerHandshaked)

  override def receive: Receive = handlePeersInfoEvents(Map.empty)

  def handlePeersInfoEvents(peersWithInfo: Map[PeerId, PeerWithInfo]): Receive = {

    case MessageFromPeer(message, peerId) if peersWithInfo.contains(peerId) =>
      val handleSentMessage = updateMaxBlock(message) _
      val PeerWithInfo(peer, oldPeerInfo) = peersWithInfo(peerId)
      val newPeerInfo = handleSentMessage(oldPeerInfo)
      context become handlePeersInfoEvents(peersWithInfo + (peer.id -> PeerWithInfo(peer, newPeerInfo)))

    case MessageToPeer(message, peerId) if peersWithInfo.contains(peerId) =>
      val PeerWithInfo(peer, oldPeerInfo) = peersWithInfo(peerId)
      val handleReceivedMessage =
        updateTotalDifficulty(message) _ andThen updateForkAccepted(message, peer) andThen updateMaxBlock(message)
      val newPeerInfo = handleReceivedMessage(oldPeerInfo)
      context become handlePeersInfoEvents(peersWithInfo + (peerId -> PeerWithInfo(peer, newPeerInfo)))

    case PeerHandshakeSuccessful(peer, peerInfo: PeerInfo) =>
      peerEventBusActor ! Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id)))
      peerEventBusActor ! Subscribe(MessageReceivedClassifier(msgsCodesWithPeerInfo, PeerSelector.WithId(peer.id)))
      context become handlePeersInfoEvents(peersWithInfo + (peer.id -> PeerWithInfo(peer, peerInfo)))

    case PeerDisconnected(peerId) if peersWithInfo.contains(peerId) =>
      peerEventBusActor ! Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peerId)))
      peerEventBusActor ! Unsubscribe(MessageReceivedClassifier(msgsCodesWithPeerInfo, PeerSelector.WithId(peerId)))
      context become handlePeersInfoEvents(peersWithInfo - peerId)

    case GetHandshakedPeers =>
      sender() ! HandshakedPeers(peersWithInfo.map{ case (_, PeerWithInfo(peer, peerInfo)) => peer -> peerInfo })

  }

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

object PeersInfoHolderActor {

  case class PeerInfo(remoteStatus: Status,
                      totalDifficulty: BigInt,
                      forkAccepted: Boolean,
                      maxBlockNumber: BigInt) extends HandshakeResult {

    def withTotalDifficulty(totalDifficulty: BigInt): PeerInfo = copy(totalDifficulty = totalDifficulty)

    def withForkAccepted(forkAccepted: Boolean): PeerInfo = copy(forkAccepted = forkAccepted)

    def withMaxBlockNumber(maxBlockNumber: BigInt): PeerInfo = copy(maxBlockNumber = maxBlockNumber)

  }

  case class PeerWithInfo(peer: Peer, peerInfo: PeerInfo)

  case object GetHandshakedPeers

  case class HandshakedPeers(peers: Map[Peer, PeerInfo])

  def props(peerEventBusActor: ActorRef, appStateStorage: AppStateStorage,
            forkResolverOpt: Option[ForkResolver]): Props =
    Props(new PeersInfoHolderActor(peerEventBusActor, appStateStorage, forkResolverOpt))

}
