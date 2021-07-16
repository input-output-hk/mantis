package io.iohk.ethereum.network.p2p

import akka.util.ByteString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.network.handshaker.EtcHelloExchangeState
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.Status
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello
import io.iohk.ethereum.network.rlpx.FrameCodec
import io.iohk.ethereum.network.rlpx.MessageCodec
import io.iohk.ethereum.utils.Config

class MessageCodecSpec extends AnyFlatSpec with Matchers {

  it should "not compress messages when remote side advertises p2p version less than 5" in new TestSetup {
    val remoteHello = remoteMessageCodec.encodeMessage(helloV4)
    messageCodec.readMessages(remoteHello)

    val localNextMessageAfterHello = messageCodec.encodeMessage(status)
    val remoteReadNotCompressedStatus = remoteMessageCodec.readMessages(localNextMessageAfterHello)

    // remote peer did not receive local status so it treats all remote messages as uncompressed
    assert(remoteReadNotCompressedStatus.size == 1)
    assert(remoteReadNotCompressedStatus.head == Right(status))
  }

  it should "compress messages when remote side advertises p2p version larger or equal 5" in new TestSetup {
    override lazy val negotiatedRemoteP2PVersion: Long = 5L
    override lazy val negotiatedLocalP2PVersion: Long = 4L

    val remoteHello = remoteMessageCodec.encodeMessage(helloV5)
    messageCodec.readMessages(remoteHello)

    val localNextMessageAfterHello = messageCodec.encodeMessage(status)
    val remoteReadNotCompressedStatus = remoteMessageCodec.readMessages(localNextMessageAfterHello)

    // remote peer did not receive local status so it treats all remote messages as uncompressed,
    // but local peer compress messages after V5 Hello message
    assert(remoteReadNotCompressedStatus.size == 1)
    assert(remoteReadNotCompressedStatus.head.isLeft)
  }

  it should "compress messages when both sides advertises p2p version larger or equal 5" in new TestSetup {
    val remoteHello = remoteMessageCodec.encodeMessage(helloV5)
    messageCodec.readMessages(remoteHello)

    val localHello = messageCodec.encodeMessage(helloV5)
    remoteMessageCodec.readMessages(localHello)

    val localNextMessageAfterHello = messageCodec.encodeMessage(status)
    val remoteReadNextMessageAfterHello = remoteMessageCodec.readMessages(localNextMessageAfterHello)

    // both peers exchanged v5 hellos, so they should send compressed messages
    assert(remoteReadNextMessageAfterHello.size == 1)
    assert(remoteReadNextMessageAfterHello.head == Right(status))
  }

  it should "compress and decompress first message after hello when receiving 2 frames" in new TestSetup {
    val remoteHello = remoteMessageCodec.encodeMessage(helloV5)
    messageCodec.readMessages(remoteHello)

    // hello won't be compressed as per spec it never is, and status will be compressed as remote peer advertised proper versions
    val localHello = messageCodec.encodeMessage(helloV5)
    val localStatus = messageCodec.encodeMessage(status)

    // both messages will be read at one, but after reading hello decompressing will be activated
    val remoteReadBothMessages = remoteMessageCodec.readMessages(localHello ++ localStatus)

    // both peers exchanged v5 hellos, so they should send compressed messages
    assert(remoteReadBothMessages.size == 2)
    assert(remoteReadBothMessages.head == Right(helloV5))
    assert(remoteReadBothMessages.last == Right(status))
  }

  trait TestSetup extends SecureChannelSetup {
    val frameCodec = new FrameCodec(secrets)
    val remoteFrameCodec = new FrameCodec(remoteSecrets)
    lazy val negotiatedRemoteP2PVersion: Long = 5L
    lazy val negotiatedLocalP2PVersion: Long = 5L

    val helloV5: Hello = Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = Config.clientId,
      capabilities = Seq(Capability.ETH63),
      listenPort = 0, //Local node not listening
      nodeId = ByteString(1)
    )

    val helloV4: Hello = helloV5.copy(p2pVersion = 4)

    val status: Status = Status(
      protocolVersion = Capability.ETH63.version,
      networkId = Config.Network.peer.networkId,
      totalDifficulty = 1,
      bestHash = ByteString(1),
      genesisHash = ByteString(1)
    )

    val decoder: MessageDecoder =
      NetworkMessageDecoder.orElse(EthereumMessageDecoder.ethMessageDecoder(Capability.ETH63))

    val messageCodec = new MessageCodec(frameCodec, decoder, negotiatedLocalP2PVersion)
    val remoteMessageCodec = new MessageCodec(remoteFrameCodec, decoder, negotiatedRemoteP2PVersion)

  }

}
