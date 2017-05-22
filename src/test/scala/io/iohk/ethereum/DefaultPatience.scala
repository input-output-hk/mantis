package io.iohk.ethereum

import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time._

trait DefaultPatience {
  self: PatienceConfiguration =>

  implicit abstract override val patienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = scaled(Span(3, Seconds)),
      interval = scaled(Span(50, Millis))
    )
}
