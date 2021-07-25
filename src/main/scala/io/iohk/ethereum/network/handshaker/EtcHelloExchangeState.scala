package io.iohk.ethereum.network.handshaker

import akka.util.ByteString

import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.utils.ServerStatus

case class EtcHelloExchangeState(handshakerConfiguration: EtcHandshakerConfiguration)
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
    // FIXME in principle this should be already negotiated
    Capability.negotiate(hello.capabilities.toList, handshakerConfiguration.blockchainConfig.capabilities) match {
      case Some(Capability.ETC64) =>
        log.debug("Negotiated protocol version with client {} is etc/64", hello.clientId)
        EtcNodeStatus64ExchangeState(handshakerConfiguration)
      case Some(Capability.ETH63) =>
        log.debug("Negotiated protocol version with client {} is eth/63", hello.clientId)
        EthNodeStatus63ExchangeState(handshakerConfiguration)
      case Some(Capability.ETH64) =>
        log.debug("Negotiated protocol version with client {} is eth/64", hello.clientId)
        EthNodeStatus64ExchangeState(handshakerConfiguration)
      case _ =>
        log.debug(
          s"Connected peer does not support {} / {} / {} protocol. Disconnecting.",
          Capability.ETH63,
          Capability.ETH64,
          Capability.ETC64
        )
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
      case ServerStatus.NotListening       => 0
    }
    Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = Config.clientId,
      capabilities = handshakerConfiguration.blockchainConfig.capabilities,
      listenPort = listenPort,
      nodeId = ByteString(nodeStatus.nodeId)
    )
  }
}

object EtcHelloExchangeState {
  val P2pVersion = 5
}
