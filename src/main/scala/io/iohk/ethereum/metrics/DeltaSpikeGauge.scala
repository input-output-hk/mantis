package io.iohk.ethereum.metrics

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

/**
  * A gauge that starts at `0` and can be triggered to go to `1`.
  * Next time it is sampled, it goes back to `0`.
  * This is normally used for either one-off signals (e.g. when an application starts)
  * or slowly re-appearing signals. Specifically, the sampling rate must be greater
  * than the rate the signal is triggered.
  */
class DeltaSpikeGauge(name: String, metrics: Metrics) {
  private[this] final val isTriggeredRef = new AtomicBoolean(false)
  private[this] final val valueRef = new AtomicInteger(0)

  private[this] def getValue(): Double = {
    if (isTriggeredRef.compareAndSet(true, false)) {
      valueRef.getAndSet(0)
    } else {
      valueRef.get()
    }
  }

  private[this] final val gauge = metrics.gauge(name, () => getValue())

  def trigger(): Unit = {
    if (isTriggeredRef.compareAndSet(false, true)) {
      valueRef.set(1)
      // Let one of the exporting metric registries pick up the `1`.
      // As soon as that happens, `getValue` will make sure that we go back to `0`.
    }
  }
}
