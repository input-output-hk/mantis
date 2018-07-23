package io.iohk.ethereum.utils

import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit

class Runner(executor: ScheduledExecutorService, interval: Long, unit: TimeUnit, f: () => Unit) extends Runnable {
  def run {
    f()
    executor.schedule(new Runner(executor, interval, unit, f),
                      interval,
                      unit)
  }
}

object Scheduler {
  def startRunner(interval: Long, unit: TimeUnit, f: () => Unit): ScheduledExecutorService = {
    val sendExecutor = Executors.newScheduledThreadPool(1);
    val sender = new Runner(sendExecutor, interval, unit, f)
    sender.run()
    sendExecutor
  }
}

