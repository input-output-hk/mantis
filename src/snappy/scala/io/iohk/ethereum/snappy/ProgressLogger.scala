package io.iohk.ethereum.snappy

import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.FiniteDuration

class ProgressLogger(targetN: BigInt, interval: FiniteDuration) extends Logger {

  private var lastUpdateMillis: Long = 0

  def start(): Unit =
    log.info(s"Starting to execute $targetN blocks")

  def update(n: BigInt): Unit = {
    val now = System.currentTimeMillis()
    if (now - lastUpdateMillis > interval.toMillis || n == targetN) {
      lastUpdateMillis = now
      val percent = f"${n.toDouble / targetN.toDouble * 100}%.1f"
      log.info(s"Executed $n ($percent%) blocks")
    }
  }
}
