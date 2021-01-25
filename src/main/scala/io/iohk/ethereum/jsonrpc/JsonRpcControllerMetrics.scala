package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.metrics.MetricsContainer
import java.util.concurrent.TimeUnit
import java.time.Duration

case object JsonRpcControllerMetrics extends MetricsContainer {

  /**
    * Counts attempts to call non-existing methods.
    */
  final val NotFoundMethodsCounter = metrics.counter("json.rpc.notfound.calls.counter")

  final val MethodsSuccessCounter = metrics.counter("json.rpc.methods.success.counter")
  final val MethodsExceptionCounter = metrics.counter("json.rpc.methods.exception.counter")
  final val MethodsErrorCounter = metrics.counter("json.rpc.methods.error.counter")

  final val HealhcheckErrorCounter = metrics.counter("json.rpc.healthcheck.error.counter")

  final val MethodsTimerName = "json.rpc.methods.timer"

  def recordMethodTime(method: String, time: Duration): Unit =
    metrics.timer("json.rpc.methods.timer", "method", method).record(time)
}
