package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.metrics.MetricsContainer
import io.micrometer.core.instrument.Counter

import java.time.Duration

case object JsonRpcControllerMetrics extends MetricsContainer {

  /** Counts attempts to call non-existing methods.
    */
  final val NotFoundMethodsCounter: Counter = metrics.counter("json.rpc.notfound.calls.counter")

  final val MethodsSuccessCounter: Counter = metrics.counter("json.rpc.methods.success.counter")
  final val MethodsExceptionCounter: Counter = metrics.counter("json.rpc.methods.exception.counter")
  final val MethodsErrorCounter: Counter = metrics.counter("json.rpc.methods.error.counter")

  final val HealhcheckErrorCounter: Counter = metrics.counter("json.rpc.healthcheck.error.counter")

  final val MethodsTimerName = "json.rpc.methods.timer"

  def recordMethodTime(method: String, time: Duration): Unit =
    metrics.timer("json.rpc.methods.timer", "method", method).record(time)
}
