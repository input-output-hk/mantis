package io.iohk.ethereum

import scala.concurrent.duration._

object Timeouts {

  implicit val normalTimeout: FiniteDuration = 3.seconds
  implicit val longTimeout: FiniteDuration = 10.seconds
  implicit val veryLongTimeout: FiniteDuration = 30.seconds

}
