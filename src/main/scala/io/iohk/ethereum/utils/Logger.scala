package io.iohk.ethereum.utils

trait Logger {
  import com.typesafe.scalalogging.Logger

  val log = Logger(getClass)
}

