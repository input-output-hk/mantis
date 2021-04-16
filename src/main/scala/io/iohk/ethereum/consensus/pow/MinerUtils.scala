package io.iohk.ethereum.consensus.pow

import akka.actor.Actor
import io.iohk.ethereum.consensus.pow.MinerResponses.MinerNotSupport

trait MinerUtils {
  self: Actor =>

  def notSupportedMockedMinerMessages: Receive = { case msg: MockedMinerProtocol =>
    sender() ! MinerNotSupport(msg)
  }
}
