package io.iohk.ethereum.ets.blockchain

import io.circe.generic.auto._
import io.circe.parser._
import io.iohk.ethereum.ets.common.ScenarioLoader

object BlockchainScenarioLoader extends ScenarioLoader[BlockchainScenario] {
  protected def parse(json: String): Map[String, BlockchainScenario] =
    decode[Map[String, BlockchainScenario]](json).fold(throw _, identity)
}
