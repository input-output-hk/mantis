package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.forkid.ForkId
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.ETH64

case class EthNodeStatus64ExchangeState(
    handshakerConfiguration: EtcHandshakerConfiguration
) extends EtcNodeStatusExchangeState[ETH64.Status] {

  import handshakerConfiguration._

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = { case status: ETH64.Status =>
    // TODO: validate fork id of the remote peer
    applyRemoteStatusMessage(RemoteStatus(status))
  }

  override protected def createStatusMsg(): MessageSerializable = {
    val bestBlockHeader = getBestBlockHeader()
    val chainWeight = blockchain.getChainWeightByHash(bestBlockHeader.hash).get
    val genesisHash = blockchainReader.genesisHeader.hash

    val status = ETH64.Status(
      protocolVersion = Capability.ETH64.version,
      networkId = peerConfiguration.networkId,
      totalDifficulty = chainWeight.totalDifficulty,
      bestHash = bestBlockHeader.hash,
      genesisHash = genesisHash,
      forkId = ForkId.create(genesisHash, blockchainConfig)(blockchainReader.getBestBlockNumber())
    )

    log.debug(s"Sending status $status")
    status
  }

}
