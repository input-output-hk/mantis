package io.iohk.ethereum.network

import cats._
import cats.implicits._
import scala.concurrent.duration.{Duration, FiniteDuration}

/** Track statistics over time a fixed size timewindow. */
class TimeSlotStats[K, V: Monoid] private (
    // Time resolution.
    val slotDuration: FiniteDuration,
    // The last written position in the buffer.
    val lastIdx: Int,
    // Ring buffer of slots statistics.
    val buffer: IndexedSeq[TimeSlotStats.Entry[K, V]]
) {
  import TimeSlotStats._

  /** Overall length of the timewindow. */
  def duration: FiniteDuration = slotDuration * slotCount
  def slotCount: Int = buffer.size

  /** Merge new stats for a given key in the current timestamp. */
  def add(key: K, stat: V, timestamp: Timestamp = System.currentTimeMillis): TimeSlotStats[K, V] = {
    val currSlotId = slotId(timestamp)
    val lastEntry = buffer(lastIdx)

    if (currSlotId == lastEntry.slotId) {
      // We're still in the same timeslot, so just append stats.
      val newEntry = lastEntry.add(key, stat)
      updated(lastIdx, newEntry)
    } else if (currSlotId > lastEntry.slotId) {
      // Go to the next slot.
      val newIdx = succ(lastIdx)
      val newEntry = Entry(currSlotId, Map(key -> stat))
      updated(newIdx, newEntry)
    } else {
      // Going backwards in time, just ignore it.
      this
    }
  }

  /** Forget all statistics about a given key. */
  def remove(key: K): TimeSlotStats[K, V] =
    updated(lastIdx, buffer.map(_.remove(key)))

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
      val entry = buffer(idx)
      if (entry.slotId < start || end < entry.slotId)
        acc
      else
        loop(pred(idx), f(acc, entry.slotStats))
    }

    loop(lastIdx, init)
  }

  /** Truncate the current timestamp based on the slot duration. */
  private def slotId(timestamp: Timestamp): Timestamp = {
    timestamp - timestamp % slotDuration.toMillis
  }

  /** The range of time slots based on the current timestamp and the buffer duration. */
  private def slotRange(timestamp: Timestamp): (Timestamp, Timestamp) = {
    val end = slotId(timestamp)
    val start = slotId(timestamp - duration.toMillis)
    start -> end
  }

  private def succ(idx: Int): Int = (idx + 1) % slotCount
  private def pred(idx: Int): Int = (idx - 1) % slotCount

  private def updated(
      lastIdx: Int,
      entry: Entry[K, V]
  ): TimeSlotStats[K, V] =
    updated(lastIdx, buffer.updated(lastIdx, entry))

  private def updated(
      lastIdx: Int,
      buffer: IndexedSeq[Entry[K, V]]
  ): TimeSlotStats[K, V] =
    new TimeSlotStats[K, V](slotDuration, lastIdx, buffer)
}

object TimeSlotStats {

  // Milliseconds since epoch.
  type Timestamp = Long

  case class Entry[K, V: Monoid](
      slotId: Timestamp,
      slotStats: Map[K, V]
  ) {
    def add(key: K, stat: V): Entry[K, V] =
      copy(slotStats = slotStats |+| Map(key -> stat))

    def remove(key: K): Entry[K, V] =
      copy(slotStats = slotStats - key)
  }

  def apply[K, V: Monoid](
      slotDuration: FiniteDuration,
      slotCount: Int
  ): Option[TimeSlotStats[K, V]] =
    if (slotDuration == Duration.Zero || slotCount <= 0) None
    else
      Some {
        new TimeSlotStats[K, V](
          slotDuration,
          lastIdx = slotCount - 1, // So the first slot we fill will move to 0.
          buffer = IndexedSeq.fill(slotCount)(Entry(0L, Map.empty[K, V]))
        )
      }
}
