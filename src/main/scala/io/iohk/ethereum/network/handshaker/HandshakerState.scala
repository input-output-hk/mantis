package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.handshaker.Handshaker.{NextMessage, HandshakeResult}
import io.iohk.ethereum.network.p2p.Message

sealed trait HandshakerState[T <: HandshakeResult]

trait InProgressState[T <: HandshakeResult] extends HandshakerState[T] {

  /**
    * Obtains the next message to be sent
    *
    * @return message to be sent with the timeout for awaiting its response
    */
  def nextMessage: NextMessage[_ <: Message]

  /**
    * Processes a message and obtains the new state of the handshake after processing it,
    * if the current state handles the received message
    *
    * @param receivedMessage, message received and to be processed by the current state
    * @return new state after the message was processed or None if the current state wasn't able to process it
    */
  def applyMessage(receivedMessage: Message): Option[HandshakerState[T]] = applyResponseMessage.lift(receivedMessage)

  /**
    * Processes a timeout to the sent message and obtains the new state of the handshake after processing it
    *
    * @return new state after the timeout was processed
    */
  def processTimeout: HandshakerState[T]

  /**
    * Function that is only defined at the messages handled by the current state, returns the new state after processing them.
    * If defined, it processes a message and obtains a new state of the handshake
    */
  protected def applyResponseMessage: PartialFunction[Message, HandshakerState[T]]

}

case class ConnectedState[T <: HandshakeResult](result: T) extends HandshakerState[T]

case class DisconnectedState[T <: HandshakeResult](reason: Int) extends HandshakerState[T]
