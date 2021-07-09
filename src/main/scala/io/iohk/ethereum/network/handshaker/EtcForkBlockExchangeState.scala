package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockHeaders
import io.iohk.ethereum.network.p2p.messages.ETH62.GetBlockHeaders
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Logger

case class EtcForkBlockExchangeState(
    handshakerConfiguration: EtcHandshakerConfiguration,
    forkResolver: ForkResolver,
    remoteStatus: RemoteStatus
) extends InProgressState[PeerInfo]
    with Logger {

  import handshakerConfiguration._

  def nextMessage: NextMessage =
    NextMessage(
      messageToSend = GetBlockHeaders(Left(forkResolver.forkBlockNumber), maxHeaders = 1, skip = 0, reverse = false),
      timeout = peerConfiguration.waitForChainCheckTimeout
    )

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = { case BlockHeaders(blockHeaders) =>
    val forkBlockHeaderOpt = blockHeaders.find(_.number == forkResolver.forkBlockNumber)

    forkBlockHeaderOpt match {
      case Some(forkBlockHeader) =>
        val fork = forkResolver.recognizeFork(forkBlockHeader)

        log.debug("Peer is running the {} fork", fork)

        if (forkResolver.isAccepted(fork)) {
          log.debug("Fork is accepted")
          //setting maxBlockNumber to 0, as we do not know best block number yet
          ConnectedState(PeerInfo.withForkAccepted(remoteStatus))
        } else {
          log.debug("Fork is not accepted")
          DisconnectedState[PeerInfo](Disconnect.Reasons.UselessPeer)
        }

      case None =>
        log.debug("Peer did not respond with fork block header")
        ConnectedState(PeerInfo.withNotForkAccepted(remoteStatus))
    }

  }

  override def respondToRequest(receivedMessage: Message): Option[MessageSerializable] = receivedMessage match {

    case GetBlockHeaders(Left(number), numHeaders, _, _) if number == forkResolver.forkBlockNumber && numHeaders == 1 =>
      log.debug("Received request for fork block")
      blockchainReader.getBlockHeaderByNumber(blockchainReader.getBestBranch(), number) match {
        case Some(header) => Some(BlockHeaders(Seq(header)))
        case None         => Some(BlockHeaders(Nil))
      }

    case _ => None

  }

  def processTimeout: HandshakerState[PeerInfo] =
    DisconnectedState(Disconnect.Reasons.TimeoutOnReceivingAMessage)

}
