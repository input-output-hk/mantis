package io.iohk.ethereum.ets.vm

import io.circe.generic.auto._
import io.circe.parser._
import io.iohk.ethereum.ets.common.ScenarioLoader

object VMScenarioLoader extends ScenarioLoader[VMScenario] {
  protected def parse(json: String): Map[String, VMScenario] =
    decode[Map[String, VMScenario]](json).fold(throw _, identity)
}
