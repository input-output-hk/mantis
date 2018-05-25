package io.iohk.ethereum.utils

import com.typesafe.scalalogging
import org.slf4j.MDC

trait Logger {
  val log = scalalogging.Logger(getClass)

  def logMDC(kv: (String, String)*)(action: scalalogging.Logger => Unit): Unit = {
    MDC.clear()
    kv.foreach { case (key, value) => MDC.put(key, value) }
    action(log)
    MDC.clear()
  }

  def logMDC(map: Map[String, String])(action: scalalogging.Logger => Unit): Unit =
    logMDC(map.toList: _*)(action)
}
