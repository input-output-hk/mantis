package io.iohk.ethereum

import akka.util.Timeout
import org.scalatest.concurrent.PatienceConfiguration

import scala.concurrent.duration._

trait NormalPatience {
  self: PatienceConfiguration =>

  implicit abstract override val patienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = Timeouts.normalTimeout,
      interval = 50.millis
    )

  implicit val actorAskTimeout: Timeout = Timeouts.normalTimeout

  implicit val taskTimeout: Duration = Timeouts.normalTimeout
}

trait LongPatience {
  self: PatienceConfiguration =>

  implicit abstract override val patienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = Timeouts.longTimeout,
      interval = 100.millis
    )

  implicit val actorAskTimeout: Timeout = Timeouts.longTimeout

  implicit val taskTimeout: Duration = Timeouts.longTimeout
}

trait VeryLongPatience {
  self: PatienceConfiguration =>

  implicit abstract override val patienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = Timeouts.veryLongTimeout,
      interval = 200.millis
    )

  implicit val actorAskTimeout: Timeout = Timeouts.veryLongTimeout

  implicit val taskTimeout: Duration = Timeouts.veryLongTimeout
}

trait MiningPatience {
  self: PatienceConfiguration =>

  implicit abstract override val patienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = Timeouts.miningTimeout,
      interval = 2000.millis
    )

  implicit val actorAskTimeout: Timeout = Timeouts.miningTimeout

  implicit val taskTimeout: Duration = Timeouts.miningTimeout
}
