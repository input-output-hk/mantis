package io.iohk.ethereum.network

import java.net.InetSocketAddress

import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters

// TODO: what if listen port changes? or server is stopped?
case class NodeInfo(key: AsymmetricCipherKeyPair, listenAddress: InetSocketAddress) {
  val nodeId = key.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
}
