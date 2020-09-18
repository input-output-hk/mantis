package io.iohk.ethereum.metrics

case class MetricsAlreadyConfiguredError(previous: Metrics, current: Metrics) extends Exception
