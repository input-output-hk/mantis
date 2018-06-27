package io.iohk.ethereum.nodebuilder

import io.iohk.ethereum.metrics.{Metrics, MetricsContainer}

class BaseNodeMetrics(metrics: Metrics) extends MetricsContainer {
  /**
   * Signifies that Mantis has started.
   */
  final val Start = metrics.deltaSpike("start.event")
}
