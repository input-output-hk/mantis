package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.forkid.ForkId
import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, RemoteStatus}
import io.iohk.ethereum.network.p2p.messages.{BaseETH6XMessages, ProtocolVersions, ETH64}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}

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
    val genesisHash = blockchain.genesisHeader.hash

    val status = ETH64.Status(
      protocolVersion = ProtocolVersions.ETH64.version,
      networkId = peerConfiguration.networkId,
      totalDifficulty = chainWeight.totalDifficulty,
      bestHash = bestBlockHeader.hash,
      genesisHash = genesisHash,
      forkId = ForkId.create(genesisHash, blockchainConfig)(blockchain.getBestBlockNumber())
    )

    log.debug(s"Sending status $status")
    status
  }

}
