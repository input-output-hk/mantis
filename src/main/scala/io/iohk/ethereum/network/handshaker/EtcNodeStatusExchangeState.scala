package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect.Reasons
import io.iohk.ethereum.utils.events._

case class EtcNodeStatusExchangeState(handshakerConfiguration: EtcHandshakerConfiguration)
  extends InProgressState[PeerInfo] with EventSupport  {

  import handshakerConfiguration._

  protected def mainService: String = "node state"

  def nextMessage: NextMessage =
    NextMessage(
      messageToSend = createStatusMsg(),
      timeout = peerConfiguration.waitForStatusTimeout
    )

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = {

    case remoteStatus: Status =>
      Event.ok("peer handshake")
        .attribute("status", remoteStatus.toString)
        .send()

      val validNetworkID = remoteStatus.networkId == handshakerConfiguration.peerConfiguration.networkId
      val validGenesisHash = remoteStatus.genesisHash == blockchain.genesisHeader.hash

      if(validNetworkID && validGenesisHash) {
        forkResolverOpt match {
          case Some(forkResolver) =>
            EtcForkBlockExchangeState(handshakerConfiguration, forkResolver, remoteStatus)
          case None =>
            ConnectedState(PeerInfo(remoteStatus, remoteStatus.totalDifficulty, true, 0))
        }
      } else
        DisconnectedState(Reasons.DisconnectRequested)

  }

  def processTimeout: HandshakerState[PeerInfo] = {
    Event.warning("peer handshake timeout").send()

    DisconnectedState(Disconnect.Reasons.TimeoutOnReceivingAMessage)
  }

  private def getBestBlockHeader() = {
    val bestBlockNumber = appStateStorage.getBestBlockNumber()
    blockchain.getBlockHeaderByNumber(bestBlockNumber).getOrElse(blockchain.genesisHeader)
  }

  private def createStatusMsg(): Status = {
    val bestBlockHeader = getBestBlockHeader()
    val totalDifficulty = blockchain.getTotalDifficultyByHash(bestBlockHeader.hash).get

    val status = Status(
      protocolVersion = Versions.PV63,
      networkId = peerConfiguration.networkId,
      totalDifficulty = totalDifficulty,
      bestHash = bestBlockHeader.hash,
      genesisHash = blockchain.genesisHeader.hash)

    status.toRiemann.send()

    status
  }

}
