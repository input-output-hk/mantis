package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, RemoteStatus}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect.Reasons
import io.iohk.ethereum.network.p2p.messages.{PV64, Versions}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}

case class EtcNodeStatus64ExchangeState(
    handshakerConfiguration: EtcHandshakerConfiguration
) extends EtcNodeStatusExchangeState[PV64.Status] {

  import handshakerConfiguration._

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = { case status: PV64.Status =>
    log.debug("Peer returned status ({})", status)

    val validNetworkID = status.networkId == handshakerConfiguration.peerConfiguration.networkId
    val validGenesisHash = status.genesisHash == blockchain.genesisHeader.hash

    if (validNetworkID && validGenesisHash) {
      forkResolverOpt match {
        case Some(forkResolver) =>
          EtcForkBlockExchangeState(handshakerConfiguration, forkResolver, RemoteStatus(status))
        case None =>
          ConnectedState(PeerInfo.withForkAccepted(RemoteStatus(status)))
      }
    } else
      DisconnectedState(Reasons.DisconnectRequested)
  }

  override protected def createStatusMsg(): MessageSerializable = {
    val bestBlockHeader = getBestBlockHeader()
    val chainWeight = blockchain.getChainWeightByHash(bestBlockHeader.hash).get

    val status = PV64.Status(
      protocolVersion = Versions.PV64,
      networkId = peerConfiguration.networkId,
      chainWeight = chainWeight,
      bestHash = bestBlockHeader.hash,
      genesisHash = blockchain.genesisHeader.hash
    )

    log.debug(s"sending status $status")
    status
  }

}
