package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Logger

case class EtcNodeStatusExchangeState(handshakerConfiguration: EtcHandshakerConfiguration) extends InProgressState[EtcPeerInfo] with Logger {

  import handshakerConfiguration._

  def nextMessage: NextMessage =
    NextMessage(
      messageToSend = createStatusMsg(),
      timeout = peerConfiguration.waitForStatusTimeout
    )

  def applyResponseMessage: PartialFunction[Message, HandshakerState[EtcPeerInfo]] = {

    case remoteStatus: Status =>
      log.info("Peer returned status ({})", remoteStatus)

      forkResolverOpt match {
        case Some(forkResolver) =>
          EtcForkBlockExchangeState(handshakerConfiguration, forkResolver, remoteStatus)
        case None =>
          ConnectedState(EtcPeerInfo(remoteStatus, remoteStatus.totalDifficulty, true, 0))
      }

  }

  def processTimeout: HandshakerState[EtcPeerInfo] = {
    log.warn("Timeout while waiting status")
    DisconnectedState(Disconnect.Reasons.TimeoutOnReceivingAMessage)
  }

  private def getBestBlockHeader() = {
    val bestBlockNumber = appStateStorage.getBestBlockNumber()
    blockchain.getBlockHeaderByNumber(bestBlockNumber).getOrElse(blockchain.genesisHeader)
  }

  private def createStatusMsg(): Status = {
    val bestBlockHeader = getBestBlockHeader()
    Status(
      protocolVersion = Versions.PV63,
      networkId = peerConfiguration.networkId,
      totalDifficulty = bestBlockHeader.difficulty,
      bestHash = bestBlockHeader.hash,
      genesisHash = blockchain.genesisHeader.hash)
  }

}
