package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.events._

case class EtcForkBlockExchangeState(
  handshakerConfiguration: EtcHandshakerConfiguration,
  forkResolver: ForkResolver, remoteStatus: Status
) extends InProgressState[PeerInfo] with EventSupport {

  import handshakerConfiguration._

  protected def mainService: String = "fork block state"

  def nextMessage: NextMessage =
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

          if (forkResolver.isAccepted(fork)) {
            Event.ok("peer fork accepted")
              .attribute("fork", fork.toString)
              .attribute("forkBlockNumber", forkResolver.forkBlockNumber.toString)
              .send()

            val peerInfo: PeerInfo = PeerInfo(remoteStatus, remoteStatus.totalDifficulty, true, forkBlockHeader.number)
            ConnectedState(peerInfo)
          } else {
            Event.warning("peer fork denied")
              .attribute("fork", fork.toString)
              .attribute("forkBlockNumber", forkResolver.forkBlockNumber.toString)
              .send()

            DisconnectedState[PeerInfo](Disconnect.Reasons.UselessPeer)
          }

        case None =>
          Event.warning("peer fork no header")
            .attribute("forkBlockNumber", forkResolver.forkBlockNumber.toString)
            .description("Peer did not respond with fork block header")
            .send()

          ConnectedState(PeerInfo(remoteStatus, remoteStatus.totalDifficulty, false, 0))
      }

  }

  override def respondToRequest(receivedMessage: Message): Option[MessageSerializable] = receivedMessage match {

    case GetBlockHeaders(Left(number), numHeaders, _, _) if number == forkResolver.forkBlockNumber && numHeaders == 1 =>
      Event.ok("peer fork block requested")
        .attribute("forkBlockNumber", forkResolver.forkBlockNumber.toString)
        .send()

      blockchain.getBlockHeaderByNumber(number) match {
        case Some(header) => Some(BlockHeaders(Seq(header)))
        case None => Some(BlockHeaders(Nil))
      }

    case _ => None

  }

  def processTimeout: HandshakerState[PeerInfo] =
    DisconnectedState(Disconnect.Reasons.TimeoutOnReceivingAMessage)

}
