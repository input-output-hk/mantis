package io.iohk.ethereum.network.handshaker

import akka.util.ByteString
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Capability, Disconnect, Hello}
import io.iohk.ethereum.utils.{Config, ServerStatus}
import io.iohk.ethereum.utils.events._


case class EtcHelloExchangeState(handshakerConfiguration: EtcHandshakerConfiguration)
  extends InProgressState[PeerInfo] with EventSupport {

  import handshakerConfiguration._

  protected def mainService: String = "hello state"

  override def nextMessage: NextMessage = {
    Event.ok("peer send hello")
      .description("RLPx connection established, sending Hello")
      .send()

    NextMessage(
      messageToSend = createHelloMsg(),
      timeout = peerConfiguration.waitForHelloTimeout
    )
  }

  override def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = {

    case hello: Hello =>
      hello.toRiemann.description("Protocol handshake finished").send()

      if (hello.capabilities.contains(Capability("eth", Versions.PV63.toByte)))
        EtcNodeStatusExchangeState(handshakerConfiguration)
      else {
        Event.warning("peer disconnecting")
          .description("Connected peer does not support eth protocol. Disconnecting.")
          .attribute("protocol", Versions.PV63.toByte.toString)
          .send()

        DisconnectedState(Disconnect.Reasons.IncompatibleP2pProtocolVersion)
      }

  }

  override def processTimeout: HandshakerState[PeerInfo] = {
    Event.warning("peer handshake timeout").attribute("type", "hello").send()

    DisconnectedState(Disconnect.Reasons.TimeoutOnReceivingAMessage)
  }

  private def createHelloMsg(): Hello = {
    val nodeStatus = nodeStatusHolder()
    val listenPort = nodeStatus.serverStatus match {
      case ServerStatus.Listening(address) => address.getPort
      case ServerStatus.NotListening => 0
    }
    Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = Config.clientId,
      capabilities = Seq(Capability("eth", Versions.PV63.toByte)),
      listenPort = listenPort,
      nodeId = ByteString(nodeStatus.nodeId)
    )
  }
}

object EtcHelloExchangeState {
  val P2pVersion = 4
}
