package io.iohk.ethereum.utils

import java.net.InetSocketAddress

import io.iohk.ethereum.network._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters

sealed trait ServerStatus
object ServerStatus {
  case object NotListening extends ServerStatus
  case class Listening(address: InetSocketAddress) extends ServerStatus
}

case class NodeStatus(
    key: AsymmetricCipherKeyPair,
    serverStatus: ServerStatus) {

  val nodeId = key.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
}
