package io.iohk.ethereum.network

import cats._
import cats.implicits._
import java.time.Clock
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.annotation.tailrec

/** Track statistics over time a fixed size timewindow. */
class TimeSlotStats[K, V: Monoid] private (
    // Time resolution.
    val slotDuration: FiniteDuration,
    // The last written position in the buffer.
    val lastIdx: Int,
    // Ring buffer of slots statistics.
    val buffer: TimeSlotStats.Buffer[K, V]
)(implicit clock: Clock) {
  import TimeSlotStats._

  /** Overall length of the timewindow. */
  def duration: FiniteDuration = slotDuration * slotCount
  def slotCount: Int = buffer.size

  /** Merge new stats for a given key in the current timestamp. */
  def add(key: K, stat: V): TimeSlotStats[K, V] = {
    val currSlotId = slotId(currentTimeMillis)
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
  def remove(key: K): TimeSlotStats[K, V] = {
    updated(lastIdx, buffer.map { case (k, v) => k -> v.remove(key) })
  }

  /** Aggregate stats for a key in all slots that are within the duration. */
  def get(key: K, window: Option[Duration] = None): V =
    fold(Monoid[V].empty, window getOrElse duration) { case (acc, stats) =>
      stats.get(key).map(acc |+| _).getOrElse(acc)
    }

  /** Aggregate all stats in all slots within the duration. */
  def getAll(window: Option[Duration] = None): Map[K, V] =
    fold(Map.empty[K, V], window getOrElse duration) { case (acc, stats) =>
      acc |+| stats
    }

  private def fold[A](init: A, window: Duration)(f: (A, Map[K, V]) => A) = {
    val (start, end) = slotRange(currentTimeMillis, window)

    @tailrec
    def loop(idx: Int, acc: List[Map[K, V]]): List[Map[K, V]] = {
      val entry = buffer(idx)
      if (entry.slotId < start || end < entry.slotId)
        acc
      else {
        val nextAcc = entry.slotStats :: acc
        val nextIdx = pred(idx)
        if (nextIdx == lastIdx) nextAcc else loop(nextIdx, nextAcc)
      }
    }

    loop(lastIdx, Nil).foldLeft(init)(f)
  }

  private def currentTimeMillis: Timestamp =
    clock.millis()

  /** Truncate the current timestamp based on the slot duration. */
  private def slotId(timestamp: Timestamp): Timestamp = {
    timestamp - timestamp % slotDuration.toMillis
  }

  /** The range of time slots based on the current timestamp and the buffer duration. */
  def slotRange(timestamp: Timestamp, window: Duration): (Timestamp, Timestamp) = {
    val end = slotId(timestamp)
    val start = slotId(timestamp - window.toMillis)
    start -> end
  }

  private def succ(idx: Int): Int = (idx + 1) % slotCount
  private def pred(idx: Int): Int = if (idx == 0) slotCount - 1 else idx - 1

  private def updated(
      lastIdx: Int,
      entry: Entry[K, V]
  ): TimeSlotStats[K, V] =
    updated(lastIdx, buffer.updated(lastIdx, entry))

  private def updated(
      lastIdx: Int,
      buffer: Buffer[K, V]
  ): TimeSlotStats[K, V] =
    new TimeSlotStats[K, V](slotDuration, lastIdx, buffer)
}

object TimeSlotStats {

  // Milliseconds since epoch.
  type Timestamp = Long
  // Using Map as a persistent ringbuffer because of frequent updates.
  type Buffer[K, V] = Map[Int, Entry[K, V]]

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
  )(implicit clock: Clock): Option[TimeSlotStats[K, V]] =
    if (slotDuration == Duration.Zero || slotCount <= 0) None
    else
      Some {
        new TimeSlotStats[K, V](
          slotDuration,
          lastIdx = slotCount - 1, // So the first slot we fill is going to be 0.
          buffer = Range(0, slotCount).map(_ -> Entry(0L, Map.empty[K, V])).toMap
        )
      }
}
