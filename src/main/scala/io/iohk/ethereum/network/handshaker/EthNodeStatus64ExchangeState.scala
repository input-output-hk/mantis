package io.iohk.ethereum.network.handshaker

import cats.effect.SyncIO

import io.iohk.ethereum.forkid.Connect
import io.iohk.ethereum.forkid.ForkId
import io.iohk.ethereum.forkid.ForkIdValidator
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.ETH64
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect

case class EthNodeStatus64ExchangeState(
    handshakerConfiguration: EtcHandshakerConfiguration
) extends EtcNodeStatusExchangeState[ETH64.Status] {

  import handshakerConfiguration._

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = { case status: ETH64.Status =>
    import ForkIdValidator.syncIoLogger
    (for {
      validationResult <-
        ForkIdValidator.validatePeer[SyncIO](blockchainReader.genesisHeader.hash, blockchainConfig)(
          blockchainReader.getBestBlockNumber(),
          status.forkId
        )
    } yield validationResult match {
      case Connect => applyRemoteStatusMessage(RemoteStatus(status))
      case _       => DisconnectedState[PeerInfo](Disconnect.Reasons.UselessPeer)
    }).unsafeRunSync()
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
