package io.iohk.ethereum.network.rlpx

import akka.actor.{ActorSystem, Props}
import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.{Timeouts, crypto}
import io.iohk.ethereum.network.p2p.Message.Version
import io.iohk.ethereum.network.p2p.{MessageDecoder, MessageSerializable}
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Ping
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.FiniteDuration

class RLPxConnectionHandlerSpec extends FlatSpec with Matchers with MockFactory {

  it should "write messages send to TCP connection" in new TestSetup {

    setupRLPxConnection()

    (mockMessageCodec.encodeMessage _).expects(Ping(): MessageSerializable).returning(ByteString("ping encoded"))
    rlpxConnection ! RLPxConnectionHandler.SendMessage(Ping())
    connection.expectMsg(Tcp.Write(ByteString("ping encoded"), RLPxConnectionHandler.Ack))

  }

  it should "write messages to TCP connection once all previous ACK were received" in new TestSetup {

    (mockMessageCodec.encodeMessage _).expects(Ping(): MessageSerializable).returning(ByteString("ping encoded")).anyNumberOfTimes()

    setupRLPxConnection()

    //Send first message
    rlpxConnection ! RLPxConnectionHandler.SendMessage(Ping())
    connection.expectMsg(Tcp.Write(ByteString("ping encoded"), RLPxConnectionHandler.Ack))
    rlpxConnection ! RLPxConnectionHandler.Ack
    connection.expectNoMsg()

    //Send second message
    rlpxConnection ! RLPxConnectionHandler.SendMessage(Ping())
    connection.expectMsg(Tcp.Write(ByteString("ping encoded"), RLPxConnectionHandler.Ack))
    rlpxConnection ! RLPxConnectionHandler.Ack
    connection.expectNoMsg()
  }

  it should "accummulate messages and write them when receiving ACKs" in new TestSetup {

    (mockMessageCodec.encodeMessage _).expects(Ping(): MessageSerializable).returning(ByteString("ping encoded")).anyNumberOfTimes()

    setupRLPxConnection()

    //Send several messages
    rlpxConnection ! RLPxConnectionHandler.SendMessage(Ping())
    rlpxConnection ! RLPxConnectionHandler.SendMessage(Ping())
    rlpxConnection ! RLPxConnectionHandler.SendMessage(Ping())

    //Only first message is sent
    connection.expectMsg(Tcp.Write(ByteString("ping encoded"), RLPxConnectionHandler.Ack))
    connection.expectNoMsg()

    //Send Ack, second message should now be sent through TCP connection
    rlpxConnection ! RLPxConnectionHandler.Ack
    connection.expectMsg(Tcp.Write(ByteString("ping encoded"), RLPxConnectionHandler.Ack))
    connection.expectNoMsg()

    //Send Ack, third message should now be sent through TCP connection
    rlpxConnection ! RLPxConnectionHandler.Ack
    connection.expectMsg(Tcp.Write(ByteString("ping encoded"), RLPxConnectionHandler.Ack))
    connection.expectNoMsg()
  }

  it should "close the connection when Ack timeout happens" in new TestSetup {
    (mockMessageCodec.encodeMessage _).expects(Ping(): MessageSerializable).returning(ByteString("ping encoded")).anyNumberOfTimes()

    setupRLPxConnection()

    rlpxConnectionParent watch rlpxConnection

    rlpxConnection ! RLPxConnectionHandler.SendMessage(Ping())
    connection.expectMsg(Tcp.Write(ByteString("ping encoded"), RLPxConnectionHandler.Ack))

    //The rlpx connection is closed after a timeout happens (after rlpxConfiguration.waitForTcpAckTimeout) and it is processed
    rlpxConnectionParent.expectTerminated(rlpxConnection, max = rlpxConfiguration.waitForTcpAckTimeout + Timeouts.normalTimeout)
  }

  it should "ignore timeout of old messages" in new TestSetup {
    (mockMessageCodec.encodeMessage _).expects(Ping(): MessageSerializable).returning(ByteString("ping encoded")).anyNumberOfTimes()

    setupRLPxConnection()

    rlpxConnection ! RLPxConnectionHandler.SendMessage(Ping()) //With SEQ number 0
    rlpxConnection ! RLPxConnectionHandler.SendMessage(Ping()) //With SEQ number 1

    //Only first Ping is sent
    connection.expectMsg(Tcp.Write(ByteString("ping encoded"), RLPxConnectionHandler.Ack))

    //Upon Ack, the next message is sent
    rlpxConnection ! RLPxConnectionHandler.Ack
    connection.expectMsg(Tcp.Write(ByteString("ping encoded"), RLPxConnectionHandler.Ack))

    //AckTimeout for the first Ping is received
    rlpxConnection ! RLPxConnectionHandler.AckTimeout(0) //AckTimeout for first Ping message

    //Connection should continue to work perfectly
    rlpxConnection ! RLPxConnectionHandler.SendMessage(Ping())
    rlpxConnection ! RLPxConnectionHandler.Ack
    connection.expectMsg(Tcp.Write(ByteString("ping encoded"), RLPxConnectionHandler.Ack))
  }

  trait TestSetup extends MockFactory with SecureRandomBuilder {
    implicit val system = ActorSystem("RLPxHandlerSpec_System")

    //Mock parameters for RLPxConnectionHandler
    val nodeKey = crypto.generateKeyPair(secureRandom)
    val mockMessageDecoder = new MessageDecoder {
      override def fromBytes(`type`: Int, payload: Array[Byte], protocolVersion: Version) =
        throw new Exception("Mock message decoder fails to decode all messages")
    }
    val protocolVersion = Versions.PV63
    val mockHandshaker = mock[AuthHandshaker]
    val connection = TestProbe()
    val mockMessageCodec = mock[MessageCodec]

    val rlpxConfiguration = new RLPxConfiguration {
      override val waitForTcpAckTimeout: FiniteDuration = Timeouts.normalTimeout
      override val waitForHandshakeTimeout: FiniteDuration = Timeouts.normalTimeout
    }

    val rlpxConnectionParent = TestProbe()
    val rlpxConnection = TestActorRef(
      Props(new RLPxConnectionHandler(
        nodeKey, mockMessageDecoder, protocolVersion, mockHandshaker, (_, _, _) => mockMessageCodec, rlpxConfiguration)),
      rlpxConnectionParent.ref)

    //Setup for RLPxConnection, after it the RLPxConnectionHandler is in a handshaked state
    def setupRLPxConnection(): Unit = {
      //Start setting up connection
      rlpxConnection ! RLPxConnectionHandler.HandleConnection(connection.ref)
      connection.expectMsgClass(classOf[Tcp.Register])

      //AuthHandshaker handles initial message
      val data = ByteString((0 until AuthHandshaker.InitiatePacketLength).map(_.toByte).toArray)
      val response = ByteString("response data")
      (mockHandshaker.handleInitialMessage _).expects(data).returning((response, AuthHandshakeSuccess(mock[Secrets], ByteString())))
      (mockMessageCodec.readMessages _).expects(ByteString.empty).returning(Nil) //For processing of messages after handshaking finishes

      rlpxConnection ! Tcp.Received(data)
      connection.expectMsg(Tcp.Write(response))

      //Connection fully established
      rlpxConnectionParent.expectMsgClass(classOf[RLPxConnectionHandler.ConnectionEstablished])
    }
  }

}
