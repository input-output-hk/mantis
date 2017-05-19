package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.PeerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Logger

case class EtcForkBlockExchangeState(handshakerConfiguration: EtcHandshakerConfiguration,
                                     forkResolver: ForkResolver, remoteStatus: Status) extends InProgressState[PeerInfo] with Logger {

  import handshakerConfiguration._

  def nextMessage: NextMessage[GetBlockHeaders] =
    NextMessage(
      messageToSend = GetBlockHeaders(Left(forkResolver.forkBlockNumber), maxHeaders = 1, skip = 0, reverse = false),
      timeout = peerConfiguration.waitForChainCheckTimeout
    )

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = {

    case BlockHeaders(blockHeaders) =>

      val forkBlockHeaderOpt = blockHeaders.find(_.number == forkResolver.forkBlockNumber)

      forkBlockHeaderOpt match {
        case Some(forkBlockHeader) =>
          val fork = forkResolver.recognizeFork(forkBlockHeader)

          log.info("Peer is running the {} fork", fork)

          if (forkResolver.isAccepted(fork)) {
            log.info("Fork is accepted")
            val peerInfo: PeerInfo = PeerInfo(remoteStatus, forkBlockHeader.number, true)
            ConnectedState(peerInfo)
          } else {
            log.warn("Fork is not accepted")
            DisconnectedState[PeerInfo](Disconnect.Reasons.UselessPeer)
          }

        case None =>
          log.info("Peer did not respond with fork block header")
          ConnectedState[PeerInfo](PeerInfo(remoteStatus, 0, false))
      }

  }

  def processTimeout: HandshakerState[PeerInfo] =
    DisconnectedState[PeerInfo](Disconnect.Reasons.TimeoutOnReceivingAMessage)

}
