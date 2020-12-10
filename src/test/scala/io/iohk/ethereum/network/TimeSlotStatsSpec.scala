package io.iohk.ethereum.network

import org.scalatest.flatspec.AnyFlatSpec
import scala.concurrent.duration._
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inspectors
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen, Shrink}, Arbitrary.arbitrary
import cats.kernel.Monoid

class TimeSlotStatsSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  import TimeSlotStatsSpec._

  behavior of "TimeSlotStats"

  val emptyStats = TimeSlotStats[String, Int](slotDuration = 1.minute, slotCount = 30).get

  it should "add new keys to the last timeslot" in {
    val stats = emptyStats.add("foo", 1)
    stats.buffer(0).slotStats("foo") shouldBe 1
  }

  it should "merge keys in the last timeslot" in {
    val stats = emptyStats.add("foo", 1).add("foo", 2).add("bar", 0, timestamp = System.currentTimeMillis + 10)
    stats.buffer(0).slotStats("foo") shouldBe 3
    stats.buffer(0).slotStats("bar") shouldBe 0
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

    stats.buffer(0).slotStats("foo") shouldBe 1
    stats.buffer(1).slotStats("foo") shouldBe 2
  }

  it should "remove keys from all slots" in {
    val stats = emptyStats
      .add("foo", 1)
      .add("bar", 2)
      .add("foo", 3, timestamp = System.currentTimeMillis + emptyStats.slotDuration.toMillis + 1)
      .add("bar", 4, timestamp = System.currentTimeMillis + emptyStats.slotDuration.toMillis + 2)
      .remove("foo")

    Inspectors.forAll(stats.buffer) { entry =>
      entry.slotStats should not contain key("foo")
    }
    Inspectors.forExactly(2, stats.buffer) { entry =>
      entry.slotStats should contain key ("bar")
    }
  }

  it should "turn around and overwrite the first slot after all of them have been written to" in {
    val stats = Range
      .inclusive(0, emptyStats.slotCount)
      .map(i => i -> (System.currentTimeMillis + emptyStats.slotDuration.toMillis * i))
      .foldLeft(emptyStats) { case (stats, (i, timestamp)) =>
        stats.add("foo", i, timestamp)
      }

    stats.buffer(0).slotStats("foo") shouldBe emptyStats.slotCount
    stats.buffer(1).slotStats("foo") shouldBe 1
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

  it should "aggregate stats in the past within the window" in new AggregateFixture {
    stats.getAll(ts2 + slotMillis * 2) should not be empty
  }

  it should "not aggregate beyond the window" in new AggregateFixture {
    stats.getAll(ts2 + slotMillis * (stats.slotCount + 1)) shouldBe empty
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

  it should "aggregate Int" in {
    testRandomAggregation[String, Int](_ + _)
  }

  it should "aggregate Boolean" in {
    implicit val boolMonoid: Monoid[Boolean] =
      Monoid.instance[Boolean](false, _ || _)
    testRandomAggregation[Int, Boolean](_ || _)
  }

  it should "aggregate Set" in {
    testRandomAggregation[Int, Set[Int]](_ union _)
  }

  def testRandomAggregation[K: Arbitrary, V: Arbitrary: Monoid](f: (V, V) => V): Unit = {
    forAll(genTimeSlotStats[K, V]) { case (stats, timestamp) =>
      val (start, end) = stats.slotRange(timestamp)
      val all = stats.getAll(timestamp)

      if (stats.buffer.exists(_.slotStats.nonEmpty)) {
        all should not be empty
      } else {
        all shouldBe empty
      }

      Inspectors.forAll(all.keySet) { key =>
        val keyStats = stats.buffer.collect {
          case entry if start <= entry.slotId && entry.slotId <= end =>
            entry.slotStats.get(key)
        }.flatten

        val expected = keyStats.reduce(f)

        all(key) shouldBe expected
        stats.get(key, timestamp) shouldBe all(key)
      }
    }
  }
}

object TimeSlotStatsSpec {
  implicit def noShrink[T]: Shrink[T] =
    Shrink[T](_ => Stream.empty)

  def genTimeSlotStats[K: Arbitrary, V: Arbitrary: Monoid]: Gen[(TimeSlotStats[K, V], TimeSlotStats.Timestamp)] =
    for {
      slotDuration <- Gen.choose(1, 5 * 60).map(_.seconds)
      slotCount <- Gen.choose(1, 30)
      keyCount <- Gen.choose(1, 5)
      keys <- Gen.listOfN(keyCount, arbitrary[K])
      eventCount <- Gen.choose(0, 100)
      timestamp = System.currentTimeMillis
      event = for {
        d <- Gen.choose(0, 10 * 60).map(_.seconds)
        k <- Gen.oneOf(keys)
        v <- arbitrary[V]
      } yield (d, k, v)
      events <- Gen.listOfN(eventCount, event)
      empty = TimeSlotStats[K, V](slotDuration, slotCount).get
      statsAndTime = events.foldLeft(empty -> System.currentTimeMillis) {
        case ((stats, timestamp), (duration, key, stat)) =>
          val nextTimestamp = timestamp + duration.toMillis
          val nextStats = stats.add(key, stat, nextTimestamp)
          nextStats -> nextTimestamp
      }
    } yield statsAndTime
}
