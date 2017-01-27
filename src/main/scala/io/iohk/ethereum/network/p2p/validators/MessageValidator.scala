package io.iohk.ethereum.network.p2p.validators

import io.iohk.ethereum.network.p2p.Message

/**
  * Trait to be used to validate messages received from other peers
  * @tparam M Messate to validate
  * @tparam E Error to be returned if message isn't valid
  */
trait MessageValidator[M <: Message, E <: MessageValidatorError] {
  def validate(message: M): Option[E]
}

trait MessageValidatorError
