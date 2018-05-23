package io.iohk.ethereum.metrics

import com.timgroup.statsd.StatsDClientErrorHandler
import io.iohk.ethereum.utils.Logger

final class MetricsErrorHandler extends StatsDClientErrorHandler with Logger {
  def handle(exception: Exception): Unit =
    log.error("[" + classOf[StatsDClientErrorHandler].getSimpleName + "]", exception)
}
