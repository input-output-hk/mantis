package io.iohk.ethereum.network.p2p

import java.net.URI

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network._
import io.iohk.ethereum.utils.ByteUtils
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex
import scorex.core.network.AuthHandshakeSuccess

trait SecureChannelSetup {

  val remoteNodeKey: AsymmetricCipherKeyPair = generateKeyPair()
  val remoteEphemeralKey: AsymmetricCipherKeyPair = generateKeyPair()
  val remoteNonce: ByteString = randomNonce()
  val remoteNodeId: Array[Byte] = remoteNodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
  val remoteUri = new URI(s"enode://${Hex.toHexString(remoteNodeId)}@127.0.0.1:30303")

  val nodeKey: AsymmetricCipherKeyPair = generateKeyPair()
  val ephemeralKey: AsymmetricCipherKeyPair = generateKeyPair()
  val nonce: ByteString = randomNonce()

  val handshaker = AuthHandshaker(nodeKey, nonce, ephemeralKey)
  val remoteHandshaker = AuthHandshaker(remoteNodeKey, remoteNonce, remoteEphemeralKey)

  val (initPacket, handshakerInitiated) = handshaker.initiate(remoteUri)
  val (responsePacket, AuthHandshakeSuccess(remoteSecrets: Secrets)) = remoteHandshaker.handleInitialMessage(initPacket)
  val AuthHandshakeSuccess(secrets: Secrets) = handshakerInitiated.handleResponseMessage(responsePacket)

  def randomNonce(): ByteString = ByteString(ByteUtils.secureRandomBytes(AuthHandshaker.NonceSize))

}
