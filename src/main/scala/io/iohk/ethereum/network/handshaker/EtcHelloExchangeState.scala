package io.iohk.ethereum.network.handshaker

import akka.util.ByteString
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities
import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities._
import io.iohk.ethereum.network.p2p.messages.{Capability, ProtocolVersions}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Disconnect, Hello}
import io.iohk.ethereum.utils.{Config, Logger, ServerStatus}

case class EtcHelloExchangeState(handshakerConfiguration: EtcHandshakerConfiguration, capability: Capability)
    extends InProgressState[PeerInfo]
    with Logger {

  import handshakerConfiguration._

  override def nextMessage: NextMessage = {
    log.debug("RLPx connection established, sending Hello")
    NextMessage(
      messageToSend = createHelloMsg(),
      timeout = peerConfiguration.waitForHelloTimeout
    )
  }

  override def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = { case hello: Hello =>
    log.debug("Protocol handshake finished with peer ({})", hello)
    capability match {
      case ProtocolVersions.ETC64 => EtcNodeStatus64ExchangeState(handshakerConfiguration)
      case ProtocolVersions.ETH63 => EtcNodeStatus63ExchangeState(handshakerConfiguration)
      case _ =>
        log.debug(s"Connected peer does not support ${ProtocolVersions.ETH63} / ${ProtocolVersions.ETC64} protocol. Disconnecting.")
        DisconnectedState(Disconnect.Reasons.IncompatibleP2pProtocolVersion)
    }
  }

  override def processTimeout: HandshakerState[PeerInfo] = {
    log.debug("Timeout while waiting for Hello")
    DisconnectedState(Disconnect.Reasons.TimeoutOnReceivingAMessage)
  }

  private def createHelloMsg(): Hello = {
    val nodeStatus = nodeStatusHolder.get()
    val listenPort = nodeStatus.serverStatus match {
      case ServerStatus.Listening(address) => address.getPort
      case ServerStatus.NotListening => 0
    }
    Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = Config.clientId,
      capabilities = handshakerConfiguration.capabilities,
      listenPort = listenPort,
      nodeId = ByteString(nodeStatus.nodeId)
    )
  }
}

object EtcHelloExchangeState {
  val P2pVersion = 5
}
