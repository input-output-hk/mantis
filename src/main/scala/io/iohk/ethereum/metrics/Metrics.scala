package io.iohk.ethereum.metrics

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.typesafe.config.Config
import io.iohk.ethereum.buildinfo.MantisBuildInfo
import io.iohk.ethereum.utils.events.EventAttr
import io.iohk.ethereum.utils.{Logger, Riemann}
import io.micrometer.core.instrument._
import io.micrometer.core.instrument.binder.jvm.{JvmGcMetrics, JvmMemoryMetrics, JvmThreadMetrics}
import io.micrometer.core.instrument.binder.logging.LogbackMetrics
import io.micrometer.core.instrument.binder.system.{ProcessorMetrics, UptimeMetrics}
import io.micrometer.core.instrument.composite.CompositeMeterRegistry
import io.micrometer.core.instrument.config.MeterFilter
import io.micrometer.jmx.JmxMeterRegistry
import io.micrometer.statsd.StatsdMeterRegistry
import io.riemann.riemann.client.IRiemannClient

case class Percentile(number: Double, name: String)

/**
 * Defines the desired client-side percentiles and the needed precision.
 *
 * @param percentiles Client-side computed percentiles.
 * @param precision Percentile precision, 1 is also the library default.
 *                  Beware of memory consumption if > 1.
 *
 * @see http://micrometer.io/docs/concepts#_memory_footprint_estimation
 *
 * @see https://github.com/micrometer-metrics/micrometer/issues/516
 */
case class Percentiles(percentiles: Seq[Percentile], precision: Int = 1) {
  val byNumber = (for {p ← percentiles} yield (p.number, p)).toMap
  val numbers = percentiles.map(_.number)
}

case class Metrics(prefix: String, percentiles: Percentiles, registry: CompositeMeterRegistry) {
  private[this] final val PrefixDot = prefix + "."

  private[this] def mkName(name: String): String = if(name.startsWith(PrefixDot)) name else PrefixDot + name

  def close(): Unit = registry.close()

  def deltaSpike(name: String): DeltaSpikeGauge =
    new DeltaSpikeGauge(mkName(name), this)

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
      .publishPercentileHistogram(false) // Set to `true` for Prometheus/Atlas backends
      .publishPercentiles(percentiles.numbers:_*)
      .percentilePrecision(percentiles.precision)
      .register(registry)

  /**
   * Returns a [[io.micrometer.core.instrument.DistributionSummary DistributionSummary]].
   * Its actual name is the concatenation of three items:
   *  [[io.iohk.ethereum.metrics.Metrics.prefix prefix]], `.`, and the value of the `name` parameter.
   */
  def distribution(name: String): DistributionSummary =
    DistributionSummary
      .builder(mkName(name))
      .publishPercentileHistogram(false) // Set to `true` for Prometheus/Atlas backends
      .publishPercentiles(percentiles.numbers:_*)
      .percentilePrecision(percentiles.precision)
      .register(registry)
}

object Metrics extends Logger {
  private[metrics] final val StdMetricsClock = Clock.SYSTEM

  // These are the percentiles we are interested in.
  // Change this list and the rest of the code should adapt.
  // Currently used in [[io.iohk.ethereum.metrics.RiemannRegistry]]
  val percentiles = Percentiles(
    List(
      Percentile(0.50, "p50"),
      Percentile(0.75, "p75"),
      Percentile(0.90, "p90"),
      Percentile(0.95, "p95"),
      Percentile(0.99, "p99"),
      Percentile(0.999, "p999")
    )
  )

  /**
   * A prefix for all metrics.
   */
  final val Prefix =  MantisBuildInfo.name
  final val PrefixDot = Prefix + "."

  //+ Metrics singleton support
  private[this] final val metrics = Metrics(Prefix, percentiles, io.micrometer.core.instrument.Metrics.globalRegistry)

  def get(): Metrics = metrics
  //- Metrics singleton support

  private[this] def onMeterAdded(m: Meter): Unit =
    log.debug(s"New ${m.getClass.getSimpleName} metric: " + m.getId.getName)

  private[this] def newJmxMeterRegistry(): JmxMeterRegistry = {
    val jmx = new JmxMeterRegistry(new MantisJmxConfig, StdMetricsClock)
    jmx.start()
    log.info(s"Started JMX registry: ${jmx}")
    jmx
  }

  private[this] def newStatsdMeterRegistry(config: MetricsConfig): StatsdMeterRegistry = {
    val statsd = new StatsdMeterRegistry(new MantisStatsdConfig(config), StdMetricsClock)
    statsd.start()
    log.info(s"Started StatsD registry: ${statsd}")
    statsd
  }

  private[this] def newRiemannRegistry(hostForEvents: String, riemannClientF: () ⇒ IRiemannClient): RiemannRegistry = {
    val config = new RiemannRegistryConfig(hostForEvents)
    val riemann = new RiemannRegistry(config, percentiles, riemannClientF)

    // We create a special thread factory that makes daemon threads of special priority
    val tfBuilder = new ThreadFactoryBuilder
    val tf = tfBuilder
      .setDaemon(true)
      .setNameFormat(classOf[RiemannRegistry].getSimpleName + "-%d")
      .setPriority(Thread.MAX_PRIORITY)
      .setUncaughtExceptionHandler((t: Thread, e: Throwable) ⇒ {
        Riemann.exception(riemann.mainService, e)
          .attribute(EventAttr.ThreadId, t.getId.toString)
          .attribute(EventAttr.ThreadName, t.getName)
          .attribute(EventAttr.ThreadPriority, t.getPriority.toString)
          .send()
      })
        .build()

    riemann.start(tf)

    log.info(s"Started Riemann registry: ${riemann}")
    riemann
  }

  /**
   * Instantiates and configures the metrics "service". This should happen once in the lifetime of the application.
   * After this call completes successfully, you can obtain the metrics service by using `Metrics.get()`.
   */
  def configure(config: MetricsConfig): Unit = {
    if(config.enabled) {
      // Expose metrics to JXM
      val jmx = newJmxMeterRegistry()
      // Expose metrics to Datadog
      val statsd = newStatsdMeterRegistry(config)
      // Expose metrics to Riemann
      val riemann = newRiemannRegistry(Riemann.hostForEvents, () ⇒ Riemann.get())

      val registry = metrics.registry

      registry.add(jmx)
      registry.add(statsd)
      registry.add(riemann)

      // Ensure that all metrics have the `Prefix`.
      // We are of course mainly interested in those that we do not control,
      // e.g. those coming from `JvmMemoryMetrics` etc.
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
      new ProcessorMetrics().bindTo(registry)
      new UptimeMetrics().bindTo(registry)
      new LogbackMetrics().bindTo(registry)
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
