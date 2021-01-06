package io.iohk.ethereum.metrics

import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.utils.LoggingUtils.getClassName
import io.kontainers.micrometer.akka.AkkaMetricRegistry
import io.micrometer.core.instrument.composite.CompositeMeterRegistry
import io.micrometer.core.instrument.config.MeterFilter
import io.micrometer.core.instrument._
import io.micrometer.prometheus.{PrometheusConfig, PrometheusMeterRegistry}
import io.prometheus.client.CollectorRegistry
import io.micrometer.jmx.JmxMeterRegistry

object MeterRegistryBuilder extends Logger {

  private[this] final val StdMetricsClock = Clock.SYSTEM

  private[this] def onMeterAdded(m: Meter): Unit =
    log.warn(s"New ${getClassName(m)} metric: " + m.getId.getName)

  /**
    * Build our meter registry consist in:
    * 1. Create each Meter registry
    * 2. Config the resultant composition
    */
  def build(metricsPrefix: String): MeterRegistry = {

    val jmxMeterRegistry = new JmxMeterRegistry(new AppJmxConfig, StdMetricsClock)

    log.info(s"Build JMX Meter Registry: ${jmxMeterRegistry}")

    val prometheusMeterRegistry =
      new PrometheusMeterRegistry(
        PrometheusConfig.DEFAULT,
        CollectorRegistry.defaultRegistry,
        StdMetricsClock
      );

    log.info(s"Build Prometheus Meter Registry: ${prometheusMeterRegistry}")
    AkkaMetricRegistry.setRegistry(prometheusMeterRegistry)

    val registry = new CompositeMeterRegistry(
      StdMetricsClock,
      java.util.Arrays.asList(jmxMeterRegistry, prometheusMeterRegistry)
    )
    // Ensure that all metrics have the `Prefix`.
    // We are of course mainly interested in those that we do not control,
    // e.g. those coming from `JvmMemoryMetrics`.
    registry
      .config()
      .meterFilter(new MeterFilter {
        override def map(id: Meter.Id): Meter.Id = {
          id.withName(MetricsUtils.mkNameWithPrefix(metricsPrefix)(id.getName))
        }
      })
      .onMeterAdded(onMeterAdded)

    registry
  }
}
