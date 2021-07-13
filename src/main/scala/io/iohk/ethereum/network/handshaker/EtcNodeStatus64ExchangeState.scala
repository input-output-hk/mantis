package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.network.p2p.messages.ETC64
import io.iohk.ethereum.network.p2p.messages.Capability

case class EtcNodeStatus64ExchangeState(
    handshakerConfiguration: EtcHandshakerConfiguration
) extends EtcNodeStatusExchangeState[ETC64.Status] {

  import handshakerConfiguration._

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = { case status: ETC64.Status =>
    applyRemoteStatusMessage(RemoteStatus(status))
  }

  override protected def createStatusMsg(): MessageSerializable = {
    val bestBlockHeader = getBestBlockHeader()
    val chainWeight = blockchain.getChainWeightByHash(bestBlockHeader.hash).get

    val status = ETC64.Status(
      protocolVersion = Capability.ETC64.version,
      networkId = peerConfiguration.networkId,
      chainWeight = chainWeight,
      bestHash = bestBlockHeader.hash,
      genesisHash = blockchainReader.genesisHeader.hash
    )

    log.debug(s"sending status $status")
    status
  }

}
