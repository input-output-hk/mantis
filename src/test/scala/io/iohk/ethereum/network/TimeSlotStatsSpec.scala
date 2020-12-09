package io.iohk.ethereum.network

import org.scalatest.flatspec.AnyFlatSpec
import scala.concurrent.duration._
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inspectors

class TimeSlotStatsSpec extends AnyFlatSpec with Matchers {
  behavior of "TimeSlotStats"

  val emptyStats = TimeSlotStats[String, Int](slotDuration = 1.minute, slotCount = 30).get

  it should "add new keys to the last timeslot" in {
    val stats = emptyStats.add("foo", 1)
    stats.statSlots(0)("foo") shouldBe 1
  }

  it should "merge keys in the last timeslot" in {
    val stats = emptyStats.add("foo", 1).add("foo", 2).add("bar", 0, timestamp = System.currentTimeMillis + 10)
    stats.statSlots(0)("foo") shouldBe 3
    stats.statSlots(0)("bar") shouldBe 0
  }

  it should "ignore updates for earlier timeslots" in {
    val stats0 = emptyStats.add("foo", 1)
    val stats1 = stats0.add("foo", 2, timestamp = System.currentTimeMillis - emptyStats.slotDuration.toMillis - 1)

    stats0 shouldBe stats1
  }

  it should "add new slots for the next timeslot" in {
    val stats = emptyStats
      .add("foo", 1)
      .add("foo", 2, timestamp = System.currentTimeMillis + emptyStats.slotDuration.toMillis + 1)

    stats.statSlots(0)("foo") shouldBe 1
    stats.statSlots(1)("foo") shouldBe 2
  }

  it should "remove keys from all slots" in {
    val stats = emptyStats
      .add("foo", 1)
      .add("bar", 2)
      .add("foo", 3, timestamp = System.currentTimeMillis + emptyStats.slotDuration.toMillis + 1)
      .add("bar", 4, timestamp = System.currentTimeMillis + emptyStats.slotDuration.toMillis + 2)
      .remove("foo")

    Inspectors.forAll(stats.statSlots) {
      _ should not contain key("foo")
    }
  }

  it should "turn around and overwrite the first slot after all of them have been written to" in {
    val stats = Range
      .inclusive(0, emptyStats.slotCount)
      .map(i => i -> (System.currentTimeMillis + emptyStats.slotDuration.toMillis * i))
      .foldLeft(emptyStats) { case (stats, (i, timestamp)) =>
        stats.add("foo", i, timestamp)
      }

    stats.statSlots(0)("foo") shouldBe emptyStats.slotCount
    stats.statSlots(1)("foo") shouldBe 1
  }

  it should "aggregate the stats of a given key" in new AggregateFixture {
    stats.get("foo") shouldBe 0
    stats.get("foo", ts1) shouldBe 0
    stats.get("bar", ts1) shouldBe 0
    stats.get("foo", ts2) shouldBe 3
    stats.get("bar", ts2) shouldBe 6
    stats.get("BAR", ts2) shouldBe 0
  }

  it should "aggregate all stats" in new AggregateFixture {
    stats.getAll() shouldBe empty
    stats.getAll(ts1) shouldBe empty
    stats.getAll(ts2) shouldBe Map("foo" -> 3, "bar" -> 6)
  }

  trait AggregateFixture {
    val slotMillis = emptyStats.slotDuration.toMillis
    val ts0 = System.currentTimeMillis
    val ts1 = ts0 + slotMillis * emptyStats.slotCount // puts t0 out of scope
    val ts2 = ts1 + slotMillis

    val stats = emptyStats
      .add("foo", 1, ts0)
      .add("bar", 2, ts1)
      .add("foo", 3, ts2)
      .add("bar", 4, ts2)

    stats.lastIdx shouldBe 2
  }

  it should "handle 0 in configuration" in {
    // This might happen if we base the values on something which can be 0.
    val zeros = List(
      TimeSlotStats[String, Int](slotDuration = 1.minutes, slotCount = 0),
      TimeSlotStats[String, Int](slotDuration = 0.minutes, slotCount = 1)
    )
    Inspectors.forAll(zeros) {
      _ shouldBe empty
    }
  }
}
