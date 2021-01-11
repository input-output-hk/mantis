package io.iohk.ethereum.metrics

import java.util.concurrent.atomic.AtomicReference

import io.micrometer.core.instrument._
import io.micrometer.core.instrument.simple.SimpleMeterRegistry
import io.prometheus.client.exporter.HTTPServer
import io.prometheus.client.hotspot.DefaultExports
import kamon.Kamon

import scala.util.Try

case class Metrics(metricsPrefix: String, registry: MeterRegistry, serverPort: Int = 0) {

  private[this] def mkName: String => String = MetricsUtils.mkNameWithPrefix(metricsPrefix)

  private lazy val server: HTTPServer = new HTTPServer(serverPort)

  def start(): Unit = {
    server // We need this to evaluate the lazy val!
    DefaultExports.initialize()
    Kamon.init()
  }

  def close(): Unit = {
    registry.close()
    server.stop()
  }

  def deltaSpike(name: String): DeltaSpikeGauge =
    new DeltaSpikeGauge(mkName(name), this)

  /**
    * Returns a [[io.micrometer.core.instrument.Gauge Gauge]].
    * @param computeValue A function that computes the current gauge value.
    */
  def gauge(name: String, computeValue: () => Double): Gauge =
    Gauge
      // Note Never use `null` as the value for the second parameter.
      //      If you do, you risk getting no metrics out of the gauge.
      //      So we just use a vanilla `this` but any other non-`null`
      //      value would also do.
      .builder(mkName(name), this, (_: Any) => computeValue())
      .register(registry)

  /**
    * Returns a [[io.micrometer.core.instrument.Counter Counter]].
    */
  def counter(name: String): Counter =
    Counter
      .builder(mkName(name))
      .register(registry)

  /**
    * Returns a [[io.micrometer.core.instrument.Timer Timer]].
    */
  def timer(name: String): Timer =
    Timer
      .builder(mkName(name))
      .register(registry)

  /**
    * Returns a [[io.micrometer.core.instrument.DistributionSummary DistributionSummary]].
    */
  def distribution(name: String): DistributionSummary =
    DistributionSummary
      .builder(mkName(name))
      .register(registry)
}

object Metrics {
  final val MetricsPrefix = "app"

  //+ Metrics singleton support
  private[this] final val metricsSentinel = Metrics(MetricsPrefix, new SimpleMeterRegistry())

  private[this] final val metricsRef = new AtomicReference[Metrics](metricsSentinel)

  private[this] def setOnce(metrics: Metrics): Boolean = metricsRef.compareAndSet(metricsSentinel, metrics)

  def get(): Metrics = metricsRef.get()
  //- Metrics singleton support

  /**
    * Instantiates and configures the metrics "service". This should happen once in the lifetime of the application.
    * After this call completes successfully, you can obtain the metrics service by using `Metrics.get()`.
    */
  def configure(config: MetricsConfig): Try[Unit] = {
    Try {
      if (config.enabled) {
        val registry = MeterRegistryBuilder.build(MetricsPrefix)
        val metrics = new Metrics(MetricsPrefix, registry, config.port)
        if (setOnce(metrics))
          metrics.start()
        else {
          metrics.close()
          throw new MetricsAlreadyConfiguredError(previous = metrics, current = get())
        }
      }
    }
  }
}
