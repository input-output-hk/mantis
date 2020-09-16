package io.iohk.ethereum.consensus.ethash

import akka.actor.Actor
import io.iohk.ethereum.consensus.ethash.MinerResponses.MinerNotSupport

trait MinerUtils {
  self: Actor =>

  def notSupportedMockedMinerMessages: Receive = {
    case msg: MockedMinerProtocol =>
      sender() ! MinerNotSupport(msg)
  }
}
