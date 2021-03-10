package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, RemoteStatus}
import io.iohk.ethereum.network.p2p.messages.{PV60, ProtocolVersions}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}

/*
  It's used by 60-63 protocols
 */
case class EtcNodeStatus60ExchangeState(
    handshakerConfiguration: EtcHandshakerConfiguration
) extends EtcNodeStatusExchangeState[PV60.Status] {

  import handshakerConfiguration._

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = { case status: PV60.Status =>
    applyRemoteStatusMessage(RemoteStatus(status))
  }

  override protected def createStatusMsg(): MessageSerializable = {
    val bestBlockHeader = getBestBlockHeader()
    val chainWeight = blockchain.getChainWeightByHash(bestBlockHeader.hash).get

    val status = PV60.Status(
      protocolVersion = ProtocolVersions.PV63,
      networkId = peerConfiguration.networkId,
      totalDifficulty = chainWeight.totalDifficulty,
      bestHash = bestBlockHeader.hash,
      genesisHash = blockchain.genesisHeader.hash
    )

    log.debug(s"sending status $status")
    status
  }

}
