package io.iohk.ethereum.utils

import java.time.{Clock, Instant, ZoneId}


class MockClock(
                 private var currentTimeMillis: Long = System.currentTimeMillis,
                 zoneId: ZoneId = ZoneId.of("UTC")
               ) extends Clock {
  def windByMillis(by: Long): Unit =
    currentTimeMillis = currentTimeMillis + by

  override def instant(): Instant = Instant.ofEpochMilli(currentTimeMillis)
  // The following are implemented for completness' sake but not used:
  override def getZone(): ZoneId = zoneId
  override def withZone(x: ZoneId): Clock = new MockClock(currentTimeMillis, zoneId)
}