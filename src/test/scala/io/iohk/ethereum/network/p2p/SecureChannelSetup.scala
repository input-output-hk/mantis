package io.iohk.ethereum.network.p2p

import java.net.URI

import akka.util.ByteString
import io.iohk.ethereum.{SecureRandomProvider, crypto}
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network._
import io.iohk.ethereum.network.rlpx.{AuthHandshakeSuccess, AuthHandshaker, Secrets}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex

trait SecureChannelSetup extends SecureRandomProvider {

  val remoteNodeKey: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val remoteEphemeralKey: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val remoteNonce: ByteString = randomNonce()
  val remoteNodeId: Array[Byte] = remoteNodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
  val remoteUri = new URI(s"enode://${Hex.toHexString(remoteNodeId)}@127.0.0.1:30303")

  val nodeKey: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val ephemeralKey: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val nonce: ByteString = randomNonce()

  val handshaker = AuthHandshaker(nodeKey, nonce, ephemeralKey, secureRandom)
  val remoteHandshaker = AuthHandshaker(remoteNodeKey, remoteNonce, remoteEphemeralKey, secureRandom)

  val (initPacket, handshakerInitiated) = handshaker.initiate(remoteUri)
  val (responsePacket, AuthHandshakeSuccess(remoteSecrets: Secrets)) = remoteHandshaker.handleInitialMessageV4(initPacket)
  val AuthHandshakeSuccess(secrets: Secrets) = handshakerInitiated.handleResponseMessageV4(responsePacket)

  def randomNonce(): ByteString = crypto.secureRandomByteString(secureRandom, AuthHandshaker.NonceSize)

}
