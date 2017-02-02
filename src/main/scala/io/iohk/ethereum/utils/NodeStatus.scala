package io.iohk.ethereum.utils

import java.net.InetSocketAddress

import akka.util.ByteString
import io.iohk.ethereum.network._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters

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
