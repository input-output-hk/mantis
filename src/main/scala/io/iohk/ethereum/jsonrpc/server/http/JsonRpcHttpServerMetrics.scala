package io.iohk.ethereum.jsonrpc.server.http

import io.iohk.ethereum.metrics.{Metrics, MetricsContainer}

class JsonRpcHttpServerMetrics(metrics: Metrics) extends MetricsContainer {
  final val RequestSizeDistribution = metrics.distribution("json.rpc.request.size.distribution")
}
