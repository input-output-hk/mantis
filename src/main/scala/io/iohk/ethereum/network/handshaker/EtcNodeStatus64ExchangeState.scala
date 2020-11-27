package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, RemoteStatus}
import io.iohk.ethereum.network.p2p.messages.{PV64, ProtocolVersions}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}

case class EtcNodeStatus64ExchangeState(
    handshakerConfiguration: EtcHandshakerConfiguration
) extends EtcNodeStatusExchangeState[PV64.Status] {

  import handshakerConfiguration._

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = { case status: PV64.Status =>
    applyRemoteStatusMessage(RemoteStatus(status))
  }

  override protected def createStatusMsg(): MessageSerializable = {
    val bestBlockHeader = getBestBlockHeader()
    val chainWeight = blockchain.getChainWeightByHash(bestBlockHeader.hash).get

    val status = PV64.Status(
      protocolVersion = ProtocolVersions.PV64,
      networkId = peerConfiguration.networkId,
      chainWeight = chainWeight,
      bestHash = bestBlockHeader.hash,
      genesisHash = blockchain.genesisHeader.hash
    )

    log.debug(s"sending status $status")
    status
  }

}
