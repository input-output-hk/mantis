package io.iohk.ethereum.utils

import com.typesafe.scalalogging
import org.slf4j.LoggerFactory
import org.slf4j.MDC

trait Logger {
  protected val log: scalalogging.Logger = com.typesafe.scalalogging.Logger(LoggerFactory.getLogger(getClass))
}

trait LazyLogger {
  protected lazy val log: scalalogging.Logger = com.typesafe.scalalogging.Logger(LoggerFactory.getLogger(getClass))
}

trait LoggingContext {
  val asParameterMap: Map[String, String]
}

object LoggingContext {
  implicit class ContextLoggerOps[T <: scalalogging.Logger](log: T) {
    def withContext(context: LoggingContext)(doLog: T => Unit): Unit = {
      context.asParameterMap.foreach { case (key, value) => MDC.put(key, value) }
      doLog(log)
      MDC.clear()
    }
  }
}
