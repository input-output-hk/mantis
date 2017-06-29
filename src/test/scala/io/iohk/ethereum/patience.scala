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
}

trait LongPatience {
  self: PatienceConfiguration =>

  implicit abstract override val patienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = Timeouts.longTimeout,
      interval = 100.millis
    )

  implicit val actorAskTimeout: Timeout = Timeouts.longTimeout
}


trait VeryLongPatience {
  self: PatienceConfiguration =>

  implicit abstract override val patienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = Timeouts.veryLongTimeout,
      interval = 200.millis
    )

  implicit val actorAskTimeout: Timeout = Timeouts.veryLongTimeout
}
