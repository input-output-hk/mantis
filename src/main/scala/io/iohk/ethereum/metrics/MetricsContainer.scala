package io.iohk.ethereum.metrics

/** An object that contains metrics, typically owned by an application component.
  * We also use it as a marker trait, so that subclasses can easily give us an idea
  * of what metrics we implement across the application.
  */
trait MetricsContainer {
  final lazy val metrics: Metrics = Metrics.get()
}
