package io.iohk.ethereum.metrics

import java.util.concurrent.atomic.AtomicReference

import com.typesafe.config.Config
import io.iohk.ethereum.utils.Logger
import io.micrometer.core.instrument._
import io.micrometer.core.instrument.binder.jvm.{JvmGcMetrics, JvmMemoryMetrics, JvmThreadMetrics}
import io.micrometer.core.instrument.binder.system.UptimeMetrics
import io.micrometer.core.instrument.composite.CompositeMeterRegistry
import io.micrometer.core.instrument.config.MeterFilter
import io.micrometer.core.instrument.simple.SimpleMeterRegistry
import io.micrometer.jmx.JmxMeterRegistry
import io.micrometer.statsd.StatsdMeterRegistry

case class Metrics(prefix: String, registry: MeterRegistry) {
  private[this] final val PrefixDot = prefix + "."

  private[this] def mkName(name: String): String = if(name.startsWith(PrefixDot)) name else PrefixDot + name

  def close(): Unit = registry.close()

  def deltaSpike(name: String): DeltaSpikeGauge =
    new DeltaSpikeGauge(name, this)

  /**
   * Returns a [[io.micrometer.core.instrument.Gauge Gauge]].
   * Its actual name is the concatenation of three items:
   *  [[io.iohk.ethereum.metrics.Metrics.prefix prefix]], `.`, and the value of the `name` parameter.
   *
   * @param computeValue A function that computes the current gauge value.
   */
  def gauge(name: String, computeValue: () ⇒ Double): Gauge =
    Gauge
      // Note Never use `null` as the value for the second parameter.
      //      If you do, you risk getting no metrics out of the gauge.
      //      So we just use a vanilla `this` but any other non-`null`
      //      value would also do.
      .builder(mkName(name), this, (_: Any) ⇒ computeValue())
      .register(registry)

  /**
   * Returns a [[io.micrometer.core.instrument.Counter Counter]].
   * Its actual name is the concatenation of three items:
   *  [[io.iohk.ethereum.metrics.Metrics.prefix prefix]], `.`, and the value of the `name` parameter.
   */
  def counter(name: String): Counter =
    Counter
      .builder(mkName(name))
      .register(registry)

  /**
   * Returns a [[io.micrometer.core.instrument.Timer Timer]].
   * Its actual name is the concatenation of three items:
   *  [[io.iohk.ethereum.metrics.Metrics.prefix prefix]], `.`, and the value of the `name` parameter.
   */
  def timer(name: String): Timer =
    Timer
      .builder(mkName(name))
      .register(registry)
}

object Metrics extends Logger {
  private[this] final val StdMetricsClock = Clock.SYSTEM

  //+ Metrics singleton support
  private[this] final val metricsSentinel = Metrics(Prefix, new SimpleMeterRegistry())

  private[this] final val metricsRef = new AtomicReference[Metrics](metricsSentinel)

  private[this] def setOnce(metrics: Metrics): Boolean = metricsRef.compareAndSet(metricsSentinel, metrics)

  def get(): Metrics = metricsRef.get()
  //- Metrics singleton support

  /**
   * A prefix for all metrics.
   */
  final val Prefix = "mantis" // TODO there are several other strings of this value. Can we consolidate?
  final val PrefixDot = Prefix + "."

  private[this] def onMeterAdded(m: Meter): Unit =
    log.debug(s"New ${m.getClass.getSimpleName} metric: " + m.getId.getName)

  /**
   * Instantiates and configures the metrics "service". This should happen once in the lifetime of the application.
   * After this call completes successfully, you can obtain the metrics service by using `Metrics.get()`.
   */
  def configure(config: MetricsConfig): Unit = {
    if(config.enabled) {
      val jmx = new JmxMeterRegistry(new MantisJmxConfig, StdMetricsClock)
      jmx.start()
      log.info(s"Started JMX registry: ${jmx}")

      val statsd = new StatsdMeterRegistry(new MantisStatsdConfig(config), StdMetricsClock)
      statsd.start()
      log.info(s"Started StatsD registry: ${statsd}")

      val registry = new CompositeMeterRegistry(StdMetricsClock, java.util.Arrays.asList(jmx, statsd))

      // Ensure that all metrics have the `Prefix`.
      // We are of course mainly interested in those that we do not control,
      // e.g. those coming from `JvmMemoryMetrics`.
      registry.config()
        .meterFilter(new MeterFilter {
          override def map(id: Meter.Id): Meter.Id = {
            val name = id.getName
            if(name.startsWith(PrefixDot)) id else id.withName(PrefixDot + name)
          }
        })
        .onMeterAdded(onMeterAdded)

      new JvmMemoryMetrics().bindTo(registry)
      new JvmGcMetrics().bindTo(registry)
      new JvmThreadMetrics().bindTo(registry)
      new UptimeMetrics().bindTo(registry)

      val metrics = new Metrics(Prefix, registry)

      if(setOnce(metrics)) {
        log.info(s"Configured metrics: $metrics")
      } else {
        val err = s"Could not configure metrics: $metrics, the current value is: ${get()}"
        log.error(err)
        metrics.close()
        throw new Exception(err)
      }
    }
  }

  /**
   * Instantiates and configures the metrics subsystem. This should happen once in the lifetime of the application.
   * After this call completes successfully, you can obtain the [[io.iohk.ethereum.metrics.Metrics Metrics]]
   * by using [[io.iohk.ethereum.metrics.Metrics#get Metrics.get]].
   */
  def configure(mantisConfig: Config): Unit = {
    val config = MetricsConfig(mantisConfig)
    configure(config)
  }
}
