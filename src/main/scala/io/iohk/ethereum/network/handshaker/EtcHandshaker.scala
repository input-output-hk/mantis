package io.iohk.ethereum.network.handshaker

import java.util.concurrent.atomic.AtomicReference

import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.p2p.Message.Version
import io.iohk.ethereum.network.p2p.messages.ProtocolNegotiator
import io.iohk.ethereum.utils.NodeStatus

case class EtcHandshaker private (
    handshakerState: HandshakerState[PeerInfo],
    handshakerConfiguration: EtcHandshakerConfiguration
) extends Handshaker[PeerInfo] {

  protected def copy(handshakerState: HandshakerState[PeerInfo]): Handshaker[PeerInfo] = {
    EtcHandshaker(handshakerState, handshakerConfiguration)
  }

}

object EtcHandshaker {

  def apply(
      handshakerConfiguration: EtcHandshakerConfiguration,
      protocolNegotiator: ProtocolNegotiator
  ): EtcHandshaker = {
    val initialState = EtcHelloExchangeState(handshakerConfiguration, protocolNegotiator)
    EtcHandshaker(initialState, handshakerConfiguration)
  }

}

trait EtcHandshakerConfiguration {
  val nodeStatusHolder: AtomicReference[NodeStatus]
  val blockchain: Blockchain
  val appStateStorage: AppStateStorage
  val peerConfiguration: PeerConfiguration
  val forkResolverOpt: Option[ForkResolver]
  val protocolVersion: Version
}
