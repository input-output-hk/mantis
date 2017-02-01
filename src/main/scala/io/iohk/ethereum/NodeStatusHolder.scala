package io.iohk.ethereum

import java.net.InetSocketAddress

import akka.actor.{ActorLogging, Props, Actor}
import akka.util.ByteString
import io.iohk.ethereum.network._
import io.iohk.ethereum.utils.Config
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex

class NodeStatusHolder(key: AsymmetricCipherKeyPair) extends Actor with ActorLogging {

  import NodeStatusHolder._

  var nodeStatus =
    NodeStatus(
      key = key,
      serverStatus = ServerStatus.NotListening,
      blockchainStatus = BlockchainStatus(Config.Blockchain.genesisDifficulty, Config.Blockchain.genesisHash))

  override def receive: Receive = {
    case GetNodeStatus =>
      sender() ! NodeStatusResponse(nodeStatus)

    case UpdateServerStatus(serverStatus) =>
      nodeStatus = nodeStatus.copy(serverStatus = serverStatus)

      serverStatus match {
        case ServerStatus.NotListening =>
          log.info("Server is not listening")
        case ServerStatus.Listening(address) =>
          log.info("Server is listening on {}", address)
          log.info("Node address: enode://{}@{}:{}",
            Hex.toHexString(nodeStatus.nodeId),
            address.getAddress.getHostAddress,
            address.getPort)
      }

    case UpdateBlockchainStatus(blockchainStatus) =>
      nodeStatus = nodeStatus.copy(blockchainStatus = blockchainStatus)
  }

}

object NodeStatusHolder {
  def props(key: AsymmetricCipherKeyPair): Props =
    Props(new NodeStatusHolder(key))

  sealed trait ServerStatus
  object ServerStatus {
    case object NotListening extends ServerStatus
    case class Listening(address: InetSocketAddress) extends ServerStatus
  }

  case class BlockchainStatus(totalDifficulty: BigInt, bestHash: ByteString)

  case class NodeStatus(
      key: AsymmetricCipherKeyPair,
      serverStatus: ServerStatus,
      blockchainStatus: BlockchainStatus) {

    val nodeId = key.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
  }

  case class UpdateServerStatus(serverStatus: ServerStatus)
  case class UpdateBlockchainStatus(blockchainStatus: BlockchainStatus)

  case object GetNodeStatus
  case class NodeStatusResponse(nodeStatus: NodeStatus)
}
