package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeComplete.{HandshakeFailure, HandshakeSuccess}
import io.iohk.ethereum.network.handshaker.Handshaker.{HandshakeComplete, HandshakeResult, NextMessage}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}
import io.iohk.ethereum.rlp.RLPEncoder

import scala.concurrent.duration.FiniteDuration

trait Handshaker[T <: HandshakeResult] {

  protected val handshakerState: HandshakerState[T]

  /**
    * Obtains the next message to be sent if the handshaking is in progress, or the result of the handshake
    *
    * @return next message to be sent or the result of the handshake
    */
  def nextMessage: Either[HandshakeComplete[T], NextMessage] = handshakerState match {
    case inProgressState: InProgressState[T] =>
      Right(inProgressState.nextMessage)
    case ConnectedState(peerInfo) =>
      Left(HandshakeSuccess(peerInfo))
    case DisconnectedState(reason: Int) =>
      Left(HandshakeFailure(reason))
  }

  /**
    * Processes a received message and obtains a new Handshaker if the handshaker handles the received message
    *
    * @param receivedMessage, message received and to be processed
    * @return handshaker after the message was processed or None if it doesn't change
    */
  def applyMessage(receivedMessage: Message): Option[Handshaker[T]] = handshakerState match {
    case inProgressState: InProgressState[T] =>
      inProgressState.applyMessage(receivedMessage).map{ newState =>
        copy(handshakerState = newState)
      }
    case _ => None
  }

  /**
    * Obtains the response to a message if there should be one.
    *
    * @param receivedMessage, message received and to be optionally responded
    * @return message to be sent as a response to the received one, if there should be any
    */
  def respondToRequest(receivedMessage: Message): Option[MessageSerializable] = handshakerState match {
    case inProgressState: InProgressState[T] =>
      inProgressState.respondToRequest(receivedMessage)
    case _ => None
  }

  /**
    * Processes a timeout to the latest message sent and obtains the new Handshaker
    *
    * @return handshaker after the timeout was processed
    */
  def processTimeout: Handshaker[T] = handshakerState match {
    case inProgressState: InProgressState[T] =>
      val newState: HandshakerState[T] = inProgressState.processTimeout
      copy(handshakerState = newState)
    case _ => this
  }

  /**
    * Obtains a Handshaker with the passed state
    *
    * @param handshakerState, for the new handshaker
    * @return handshaker with the passed state
    */
  protected def copy(handshakerState: HandshakerState[T]): Handshaker[T]

}

object Handshaker {

  trait HandshakeResult

  sealed trait HandshakeComplete[T <: HandshakeResult]

  object HandshakeComplete {
    case class HandshakeFailure[T <: HandshakeResult](reason: Int) extends HandshakeComplete[T]
    case class HandshakeSuccess[T <: HandshakeResult](result: T) extends HandshakeComplete[T]
  }

  case class NextMessage(messageToSend: MessageSerializable, timeout: FiniteDuration)

}
