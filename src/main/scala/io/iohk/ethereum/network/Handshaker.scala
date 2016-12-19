package io.iohk.ethereum.network

import java.security.SecureRandom

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECIESCoder
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params._
import org.spongycastle.util.encoders.Hex

import scala.util.Random

object Handshaker {

  val nodeId = "a node id"
  val nodeKey = generateKeyPair()

  var sessionTokenStorage = Map.empty[String, Array[Byte]]

  def handshake(host: String, port: Int, remoteNodeId: String) = {
    val remotePubKey = {
      val remoteIdBytes = Array[Byte](4.toByte) ++ Hex.decode(remoteNodeId)
      curve.getCurve.decodePoint(remoteIdBytes)
    }

    val initNonce = new Array[Byte](32)
    Random.nextBytes(initNonce)

    val authInitiateMessage = AuthInitiateMessage(nodeKey, generateKeyPair(), remotePubKey, initNonce, sessionTokenStorage.get(remoteNodeId))

    if (!sessionTokenStorage.contains(remoteNodeId))
      sessionTokenStorage += (remoteNodeId -> authInitiateMessage.sharedSecret)

    val initiatePacket = ByteString(ECIESCoder.encrypt(remotePubKey, authInitiateMessage.encode(), None))

    // send initiatePacket and wait for response
  }

  def generateKeyPair(): AsymmetricCipherKeyPair = {
    val generator = new ECKeyPairGenerator
    generator.init(new ECKeyGenerationParameters(curve, new SecureRandom))
    generator.generateKeyPair()
  }

}
