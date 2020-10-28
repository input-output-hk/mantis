package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect.Reasons
import io.iohk.ethereum.utils.Logger

case class EtcNodeStatusExchangeState(handshakerConfiguration: EtcHandshakerConfiguration)
    extends InProgressState[PeerInfo]
    with Logger {

  import handshakerConfiguration._

  def nextMessage: NextMessage =
    NextMessage(
      messageToSend = createStatusMsg(),
      timeout = peerConfiguration.waitForStatusTimeout
    )

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = { case remoteStatus: Status =>
    log.debug("Peer returned status ({})", remoteStatus)

    val validNetworkID = remoteStatus.networkId == handshakerConfiguration.peerConfiguration.networkId
    val validGenesisHash = remoteStatus.genesisHash == blockchain.genesisHeader.hash

    if (validNetworkID && validGenesisHash) {
      forkResolverOpt match {
        case Some(forkResolver) =>
          EtcForkBlockExchangeState(handshakerConfiguration, forkResolver, remoteStatus)
        case None =>
          ConnectedState(PeerInfo.withForkAccepted(remoteStatus))
      }
    } else
      DisconnectedState(Reasons.DisconnectRequested)

  }

  def processTimeout: HandshakerState[PeerInfo] = {
    log.debug("Timeout while waiting status")
    DisconnectedState(Disconnect.Reasons.TimeoutOnReceivingAMessage)
  }

  private def getBestBlockHeader() = {
    val bestBlockNumber = blockchain.getBestBlockNumber()
    blockchain.getBlockHeaderByNumber(bestBlockNumber).getOrElse(blockchain.genesisHeader)
  }

  private def createStatusMsg(): Status = {
    val bestBlockHeader = getBestBlockHeader()
    val totalDifficulty = blockchain.getTotalDifficultyByHash(bestBlockHeader.hash).get
    val latestCheckpointNumber =
      if (bestBlockHeader.number < blockchainConfig.ecip1097BlockNumber) None
      else Some(blockchain.getLatestCheckpointBlockNumber())

    val status = Status(
      protocolVersion = Versions.PV63,
      networkId = peerConfiguration.networkId,
      totalDifficulty = totalDifficulty,
      latestCheckpointNumber = latestCheckpointNumber,
      bestHash = bestBlockHeader.hash,
      genesisHash = blockchain.genesisHeader.hash
    )
    log.debug(s"sending status $status")
    status
  }

}
