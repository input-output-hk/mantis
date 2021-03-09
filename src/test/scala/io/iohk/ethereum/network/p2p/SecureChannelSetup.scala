package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network._
import io.iohk.ethereum.network.rlpx.{AuthHandshakeSuccess, AuthHandshaker, Secrets}
import io.iohk.ethereum.security.SecureRandomBuilder
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.util.encoders.Hex

import java.net.URI

trait SecureChannelSetup extends SecureRandomBuilder {

  val remoteNodeKey: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val remoteEphemeralKey: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val remoteNonce: ByteString = randomNonce()
  val remoteNodeId: Array[Byte] = remoteNodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
  val remoteUri = new URI(s"enode://${Hex.toHexString(remoteNodeId)}@127.0.0.1:30303")

  val nodeKey: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val ephemeralKey: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val nonce: ByteString = randomNonce()

  val handshaker: AuthHandshaker = AuthHandshaker(nodeKey, nonce, ephemeralKey, secureRandom)
  val remoteHandshaker: AuthHandshaker = AuthHandshaker(remoteNodeKey, remoteNonce, remoteEphemeralKey, secureRandom)

  val (initPacket, handshakerInitiated) = handshaker.initiate(remoteUri)
  val (responsePacket, AuthHandshakeSuccess(remoteSecrets: Secrets, _)) =
    remoteHandshaker.handleInitialMessageV4(initPacket)
  val AuthHandshakeSuccess(secrets: Secrets, _) = handshakerInitiated.handleResponseMessageV4(responsePacket)

  def randomNonce(): ByteString = crypto.secureRandomByteString(secureRandom, AuthHandshaker.NonceSize)

}
