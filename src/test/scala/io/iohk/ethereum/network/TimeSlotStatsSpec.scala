package io.iohk.ethereum.network

import cats._
import cats.implicits._
import cats.data.State
import cats.kernel.Monoid
import java.time.Clock
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inspectors
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen, Shrink}, Arbitrary.arbitrary
import scala.concurrent.duration._
import org.scalatest.compatible.Assertion
import io.iohk.ethereum.utils.MockClock

class TimeSlotStatsSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  import TimeSlotStatsSpec._

  behavior of "TimeSlotStats"

  it should "add new keys to the last timeslot" in test {
    for {
      stats <- add("foo", 1)
    } yield {
      stats.buffer(0).slotStats("foo") shouldBe 1
    }
  }

  it should "merge keys in the last timeslot" in test {
    for {
      _ <- add("foo", 1)
      _ <- add("foo", 2)
      _ <- windClock(10.millis)
      _ <- add("bar", 0)
      stats <- getStats
    } yield {
      stats.buffer(0).slotStats("foo") shouldBe 3
      stats.buffer(0).slotStats("bar") shouldBe 0
    }
  }

  it should "ignore updates for earlier timeslots" in test {
    for {
      stats0 <- add("foo", 1)
      _ <- windClock(-defaultSlotDuration - 1.millis)
      stats1 <- add("foo", 2)
    } yield {
      stats0.buffer shouldBe stats1.buffer
    }
  }

  it should "add new slots for the next timeslot" in test {
    for {
      _ <- add("foo", 1)
      _ <- windClock(defaultSlotDuration + 1.millis)
      _ <- add("foo", 2)
      stats <- getStats
    } yield {
      stats.buffer(0).slotStats("foo") shouldBe 1
      stats.buffer(1).slotStats("foo") shouldBe 2
    }
  }

  it should "remove keys from all slots" in test {
    for {
      _ <- add("foo", 1)
      _ <- add("bar", 2)
      _ <- windClock(defaultSlotDuration + 1.millis)
      _ <- add("foo", 3)
      _ <- windClock(1.millis)
      _ <- add("bar", 4)
      _ <- remove("foo")
      stats <- getStats
    } yield {
      Inspectors.forAll(stats.buffer.values) { entry =>
        entry.slotStats should not contain key("foo")
      }
      Inspectors.forExactly(2, stats.buffer.values) { entry =>
        entry.slotStats should contain key ("bar")
      }
    }
  }

  it should "turn around and overwrite the first slot after all of them have been written to" in test {
    for {
      _ <- Range
        .inclusive(0, defaultSlotCount)
        .map { i =>
          add("foo", i) >> windClock(defaultSlotDuration)
        }
        .toList
        .sequence
      stats <- getStats
    } yield {
      stats.buffer(0).slotStats("foo") shouldBe defaultSlotCount
      stats.buffer(1).slotStats("foo") shouldBe 1
    }
  }

  def testAggregate(f: TestState[String, Int, Assertion]) = test {
    val setup = for {
      _ <- add("foo", 1)
      _ <- windClock(defaultSlotDuration * defaultSlotCount) // puts t0 out of scope
      _ <- add("bar", 2)
      _ <- windClock(defaultSlotDuration)
      _ <- add("foo", 3)
      _ <- add("bar", 4)
      s <- getStats
    } yield {
      s.lastIdx shouldBe 2
      ()
    }

    setup >> f
  }

  it should "aggregate the stats of a given key" in testAggregate {
    for {
      stats <- getStats
    } yield {
      stats.get("foo") shouldBe 3
      stats.get("bar") shouldBe 6
      stats.get("BAR") shouldBe 0
    }
  }

  it should "aggregate all stats" in testAggregate {
    for {
      stats <- getStats
    } yield {
      stats.getAll() shouldBe Map("foo" -> 3, "bar" -> 6)
    }
  }

  it should "aggregate stats that still fall in the window" in testAggregate {
    for {
      _ <- windClock(defaultSlotDuration * 2)
      stats <- getStats
    } yield {
      stats.getAll() should not be empty
    }
  }

  it should "not aggregate beyond the window" in testAggregate {
    for {
      _ <- windClock(defaultSlotDuration * (defaultSlotCount + 1))
      stats <- getStats
    } yield {
      stats.getAll() shouldBe empty
    }
  }

  it should "handle 0 in configuration" in {
    // This might happen if we base the values on something which can be 0.
    implicit val clock: Clock = Clock.systemUTC()

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

  it should "aggregate Vector" in {
    testRandomAggregation[Int, Vector[Int]](_ ++ _)
  }

  def testRandomAggregation[K: Arbitrary, V: Arbitrary: Monoid](f: (V, V) => V): Unit = {
    forAll(genTimeSlotStats[K, V]) { case (stats, clock, window) =>
      val timestamp = clock.millis()
      val (start, end) = stats.slotRange(timestamp, window)

      val windowBuffer = stats.buffer.values
        .filter { entry =>
          start <= entry.slotId && entry.slotId <= end
        }
        .toVector
        .sortBy(_.slotId)

      val all = stats.getAll(Some(window))

      if (windowBuffer.exists(_.slotStats.nonEmpty)) {
        all should not be empty
      } else {
        all shouldBe empty
      }

      Inspectors.forAll(all.keySet) { key =>
        val keyStats = windowBuffer.flatMap {
          _.slotStats.get(key)
        }
        val expected = keyStats.reduce(f)

        all(key) shouldBe expected
        stats.get(key, Some(window)) shouldBe expected
      }
    }
  }
}

object TimeSlotStatsSpec {

  implicit val clock = new MockClock()

  type TestState[K, V, A] = State[(TimeSlotStats[K, V], MockClock), A]

  val defaultSlotDuration = 1.minute
  val defaultSlotCount = 30

  def getStats[K, V]: TestState[K, V, TimeSlotStats[K, V]] =
    State.get.map(_._1)

  def modStats[K, V](f: TimeSlotStats[K, V] => TimeSlotStats[K, V]): TestState[K, V, Unit] =
    State.modify { case (stats, clock) =>
      (f(stats), clock)
    }

  def add[K, V](key: K, value: V): TestState[K, V, TimeSlotStats[K, V]] =
    modStats[K, V](_.add(key, value)) >> getStats[K, V]

  def remove[K, V](key: K): TestState[K, V, TimeSlotStats[K, V]] =
    modStats[K, V](_.remove(key)) >> getStats[K, V]

  def windClock[K, V](by: FiniteDuration): TestState[K, V, Unit] =
    State.modify { case (stats, clock) =>
      clock.windByMillis(by.toMillis)
      (stats, clock)
    }

  def test[K, V: Monoid](s: TestState[K, V, Assertion]): Unit = {
    val stats = TimeSlotStats[K, V](defaultSlotDuration, defaultSlotCount).get
    s.run(stats -> clock).value
  }

  def genTimeSlotStats[K: Arbitrary, V: Arbitrary: Monoid]: Gen[(TimeSlotStats[K, V], MockClock, FiniteDuration)] =
    for {
      slotDuration <- Gen.choose(1, 5 * 60).map(_.seconds)
      slotCount <- Gen.choose(1, 30)
      keyCount <- Gen.choose(1, 5)
      keys <- Gen.listOfN(keyCount, arbitrary[K])
      eventCount <- Gen.choose(0, 100)
      event = for {
        d <- Gen.choose(0, 10 * 60).map(_.seconds)
        k <- Gen.oneOf(keys)
        v <- arbitrary[V]
      } yield (d, k, v)
      events <- Gen.listOfN(eventCount, event)
      empty = TimeSlotStats[K, V](slotDuration, slotCount)(Monoid[V], clock).get
      stats = events.foldLeft(empty) { case (stats, (duration, key, stat)) =>
        clock.windByMillis(duration.toMillis)
        stats.add(key, stat)
      }
      window <- Gen.choose(0, stats.duration.toSeconds * 2).map(_.seconds)
    } yield (stats, clock, window)
}
