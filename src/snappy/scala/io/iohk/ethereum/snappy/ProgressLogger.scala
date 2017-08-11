package io.iohk.ethereum.snappy

import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.FiniteDuration

class ProgressLogger(startN: BigInt, targetN: BigInt, interval: FiniteDuration) extends Logger {

  private val startTimestamp = System.currentTimeMillis()
  private var lastUpdateMillis: Long = 0

  def start(): Unit =
    log.info(s"About to execute blocks $startN through $targetN (${targetN - startN + 1} total)")

  def update(n: BigInt): Unit = {
    val now = System.currentTimeMillis()
    if (now - lastUpdateMillis > interval.toMillis || n == targetN) {
      lastUpdateMillis = now
      val percent = n.toDouble / targetN.toDouble * 100
      log.info(f"Executed blocks up to $n ($percent%.1f%%). ETA: ${eta(now, n)}")
    }
  }

  private def eta(now: Long, n: BigInt): String = {
    val elapsed = (now - startTimestamp) / 1000
    if (n - startN > 0 && elapsed > 0) {
      val r = (targetN - startN + 1).toDouble / (n - startN + 1).toDouble
      val estimated = elapsed * (r - 1)
      val h = (estimated / 3600).toInt
      val m = ((estimated % 3600) / 60).toInt
      val s = (estimated % 60).toInt
      f"$h%02d:$m%02d:$s%02d"
    } else "N/A"
  }
}
