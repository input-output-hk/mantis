package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.metrics.{Metrics, MetricsContainer}

class JsonRpcControllerMetrics(metrics: Metrics) extends MetricsContainer {

  /**
   * Counts attempts to call disabled methods.
   */
  final val DisabledMethodsCounter = metrics.counter("json.rpc.disabled.calls.counter")

  final val MethodsTimer = metrics.timer("json.rpc.methods.timer")
  final val MethodsSuccessCounter = metrics.counter("json.rpc.methods.success.counter")
  final val MethodsExceptionCounter = metrics.counter("json.rpc.methods.exception.counter")
  final val MethodsErrorCounter = metrics.counter("json.rpc.methods.error.counter")
}
