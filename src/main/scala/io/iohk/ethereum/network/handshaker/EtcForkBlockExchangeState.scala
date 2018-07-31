package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Riemann

case class EtcForkBlockExchangeState(handshakerConfiguration: EtcHandshakerConfiguration,
                                     forkResolver: ForkResolver, remoteStatus: Status) extends InProgressState[PeerInfo] {

  import handshakerConfiguration._

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

          Riemann.ok("peer fork").attribute("fork", fork.toString).send

          if (forkResolver.isAccepted(fork)) {
            Riemann.ok("peer fork accepted").send
            val peerInfo: PeerInfo = PeerInfo(remoteStatus, remoteStatus.totalDifficulty, true, forkBlockHeader.number)
            ConnectedState(peerInfo)
          } else {
            Riemann.warning("peer fork denied").send
            DisconnectedState[PeerInfo](Disconnect.Reasons.UselessPeer)
          }

        case None =>
          Riemann.warning("peer fork no header").description("Peer did not respond with fork block header").send
          ConnectedState(PeerInfo(remoteStatus, remoteStatus.totalDifficulty, false, 0))
      }

  }

  override def respondToRequest(receivedMessage: Message): Option[MessageSerializable] = receivedMessage match {

    case GetBlockHeaders(Left(number), numHeaders, _, _) if number == forkResolver.forkBlockNumber && numHeaders == 1 =>
      Riemann.ok("peer fork block requested").send
      blockchain.getBlockHeaderByNumber(number) match {
        case Some(header) => Some(BlockHeaders(Seq(header)))
        case None => Some(BlockHeaders(Nil))
      }

    case _ => None

  }

  def processTimeout: HandshakerState[PeerInfo] =
    DisconnectedState(Disconnect.Reasons.TimeoutOnReceivingAMessage)

}
