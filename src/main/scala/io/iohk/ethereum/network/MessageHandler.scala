package io.iohk.ethereum.network

import io.iohk.ethereum.network.MessageHandler.HandshakeResult
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.MessageHandler._

trait MessageHandler[R <: HandshakeResult, I <: PeerInfo] {

  /**
    * Used to notify the message handler that a message is about to be sent
    *
    * @param message about to be sent
    * @return a new MessageHandler with the action to be done regarding the message, ignore it or send it
    */
  def sendingMessage(message: Message): MessageHandlingResult[R, I]

  /**
    * Used to notify the message handler that a message is about to be received
    *
    * @param message about to be received
    * @return a new MessageHandler with the action to be done regarding the message, ignore it or receive it
    */
  def receivingMessage(message: Message): MessageHandlingResult[R, I]

  /**
    * Obtains the current saved information on the peer
    */
  val peerInfo: I

}

object MessageHandler {

  sealed trait MessageAction

  object MessageAction {
    case object TransmitMessage extends MessageAction
    case object IgnoreMessage extends MessageAction
  }

  //FIXME: Defined in the handshaker on PR 185
  trait HandshakeResult

  trait PeerInfo

  case class MessageHandlingResult[R <: HandshakeResult, I <: PeerInfo](handler: MessageHandler[R, I], action: MessageAction)
}
