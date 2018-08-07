package io.iohk.ethereum.metrics

import java.util.concurrent.atomic.AtomicReference

import com.timgroup.statsd.{NoOpStatsDClient, NonBlockingStatsDClient, StatsDClient}
import com.typesafe.config.Config
import io.iohk.ethereum.utils.Logger

object MetricsClient extends Logger {
  private[this] final val NoOpClient = new NoOpStatsDClient
  private[this] final val clientRef = new AtomicReference[StatsDClient](NoOpClient)

  private[this] def setOnce(client: StatsDClient): Boolean = clientRef.compareAndSet(NoOpClient, client)

  /** Retrieves the application-wide metrics client. */
  def get(): StatsDClient = clientRef.get()


  /** A prefix for all metrics. */
  final val Prefix = "mantis" // TODO there are several other strings of this value. Can we consolidate?

  /** Default tags we send to StatsD (actually Datadog).
    *
    * @see https://github.com/input-output-hk/iohk-ops/blob/618748e09035f7bc3e3b055818c0cde4cf1958ce/modules/production.nix#L15
    */
  object Tag {
    final val Env = "env"
    final val Depl = "depl"
  }

  def mkTag(name: String, value: String): String = s"$name:$value"

  /** Instantiates and configures the metrics client. This should happen once in the lifetime of the application.
    * After this call completes successfully, you can obtain the metrics client by using [[MetricsClient.get()]].
    */
  def configure(config: MetricsConfig): Unit = {
    val enabled = config.enabled

    if(enabled) {
      val hostname = config.host
      val port = config.port
      val queueSize = config.queueSize
      val logErrors = config.logErrors
      val constantTags = Array(
        mkTag(Tag.Env, config.environment),
        mkTag(Tag.Depl, config.deployment)
      )
      val errorHandler = if(logErrors) new MetricsErrorHandler else null // null indicates NOOP

      val client =
        new NonBlockingStatsDClient(
          Prefix,
          hostname,
          port,
          queueSize,
          constantTags,
          errorHandler
        )

      if(setOnce(client)) {
        log.info(s"Configured metrics client: $client")
      } else {
        log.warn(s"Could not configure metrics client: $client")
        client.close()
      }
    }
  }

  /** Instantiates and configures the metrics client. This should happen once in the lifetime of the application.
    * After this call completes successfully, you can obtain the metrics client by using [[MetricsClient.get()]].
    */
  def configure(mantisConfig: Config): Unit = {
    val config = MetricsConfig(mantisConfig)
    configure(config)
  }
}
