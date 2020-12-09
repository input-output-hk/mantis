package io.iohk.ethereum.network

import cats._
import cats.implicits._
import scala.concurrent.duration.{Duration, FiniteDuration}

/** Track statistics over time. */
case class TimeSlotStats[K, V: Monoid] private (
    // Time resolution.
    slotDuration: FiniteDuration,
    slotCount: Int,
    // The last written slot.
    lastIdx: Int,
    // Ring buffer of the timestamp of each slot.
    timeSlots: IndexedSeq[TimeSlotStats.Timestamp],
    // Ring buffer of statistics per slot.
    statSlots: IndexedSeq[Map[K, V]]
) {
  import TimeSlotStats._

  private def succ(idx: Int): Int = (idx + 1) % slotCount
  private def pred(idx: Int): Int = (idx - 1) % slotCount

  /** Merge new stats for a given key in the current timestamp. */
  def add(key: K, stat: V, timestamp: Timestamp = System.currentTimeMillis): TimeSlotStats[K, V] = {
    val currSlot = slotId(timestamp)
    val lastSlot = timeSlots(lastIdx)

    if (currSlot == lastSlot) {
      // We're still in the same timeslot, so just append stats.
      val newStats = statSlots(lastIdx) |+| Map(key -> stat)
      copy(
        statSlots = statSlots.updated(lastIdx, newStats)
      )
    } else if (currSlot > lastSlot) {
      // Go to the next slot.
      val newStats = Map(key -> stat)
      val newIdx = succ(lastIdx)
      copy(
        lastIdx = newIdx,
        timeSlots = timeSlots.updated(newIdx, currSlot),
        statSlots = statSlots.updated(newIdx, newStats)
      )
    } else {
      // Going backwards in time, just ignore it.
      this
    }
  }

  /** Forget all statistics about a given key. */
  def remove(key: K): TimeSlotStats[K, V] =
    copy(statSlots = statSlots.map(_ - key))

  /** Aggregate stats for a key in all slots that are within the duration. */
  def get(key: K, timestamp: Timestamp = System.currentTimeMillis): V =
    fold(timestamp)(Monoid[V].empty) { case (acc, stats) =>
      acc |+| stats.getOrElse(key, Monoid[V].empty)
    }

  /** Aggregate all stats in all slots within the duration. */
  def getAll(timestamp: Timestamp = System.currentTimeMillis): Map[K, V] =
    fold(timestamp)(Map.empty[K, V]) { case (acc, stats) =>
      acc |+| stats
    }

  private def fold[A](timestamp: Timestamp)(init: A)(f: (A, Map[K, V]) => A) = {
    val (start, end) = slotRange(timestamp)

    def loop(idx: Int, acc: A): A = {
      val t = timeSlots(idx)
      if (t < start || t > end)
        acc
      else
        loop(pred(idx), f(acc, statSlots(idx)))
    }

    loop(lastIdx, init)
  }

  /** Truncate the current timestamp based on the slot duration. */
  private def slotId(timestamp: Timestamp): Timestamp = {
    timestamp - timestamp % slotDuration.toMillis
  }

  private def slotRange(timestamp: Timestamp): (Timestamp, Timestamp) = {
    val endSlot = slotId(timestamp)
    val startSlot = slotId(timestamp - slotDuration.toMillis * slotCount)
    startSlot -> endSlot
  }
}

object TimeSlotStats {

  // Milliseconds since epoch.
  type Timestamp = Long

  def apply[K, V: Monoid](
      slotDuration: FiniteDuration,
      slotCount: Int
  ): Option[TimeSlotStats[K, V]] =
    if (slotDuration == Duration.Zero || slotCount <= 0) None
    else
      Some {
        TimeSlotStats[K, V](
          slotDuration,
          slotCount,
          lastIdx = slotCount - 1, // So the first slot we fill will move to 0.
          timeSlots = IndexedSeq.fill(slotCount)(-1L),
          statSlots = IndexedSeq.fill(slotCount)(Map.empty[K, V])
        )
      }
}
