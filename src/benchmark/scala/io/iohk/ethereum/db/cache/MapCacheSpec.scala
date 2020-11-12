package io.iohk.ethereum.db.cache

import java.util.concurrent.TimeUnit

import io.iohk.ethereum.utils.Config.NodeCacheConfig
import io.iohk.ethereum.utils.Logger
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.duration.FiniteDuration

class MapCacheSpec extends AnyFunSuite with Logger {

  val WRITES_PER_THREAD = 100000
  val READS_PER_THREAD  = 500000
  val CACHE_GET_TO_CACHE_VALUES_RATIO = 5

  test("A new MapCache performance test, according to https://github.com/input-output-hk/mantis/pull/778") {
    testMapCache(createNewCache[Int, Int](1000, 100), 1, 5)
    testMapCache(createOldCache[Int, Int](1000, 100), 1, 5)
    testMapCache(createNewCache[Int, Int](1000, 100), 5, 10)
    testMapCache(createOldCache[Int, Int](1000, 100), 5, 10)
    testMapCache(createNewCache[Int, Int](1000, 100), 10, 20)
    testMapCache(createOldCache[Int, Int](1000, 100), 10, 20)
    testMapCache(createNewCache[Int, Int](1000, 100), 20, 50)
    testMapCache(createOldCache[Int, Int](1000, 100), 20, 50)
  }

  private[this] def testMapCache(cache: Cache[Int, Int], writersCount: Int, readersCount: Int): Unit = {
    val time = System.currentTimeMillis()

    val writers = (0 until writersCount).map { _ =>
      new Thread(() => {
        (0 until WRITES_PER_THREAD).foreach { y =>
          // when readers count = 5 - a value can be one of 1, 2, 3, 4
          val value = y % readersCount
          cache.update(value :: Nil, (value -> value) :: Nil)
        }
      })
    }

    val readers = (0 until readersCount).map { x =>
      new Thread(() => {
        (0 until READS_PER_THREAD).foreach { y =>
          if (y % CACHE_GET_TO_CACHE_VALUES_RATIO == 0) {
            cache.getValues
          } else {
            cache.get(y % readersCount)
          }
        }
      })
    }

    (writers ++ readers)
      .map(th => { th.start(); th })
      .foreach(_.join())

    val elapsed = System.currentTimeMillis() - time
    log.warn(f"${cache.getClass.getSimpleName} Writers: $writersCount%3d;    Readers: $readersCount%3d.    Elapsed millis: $elapsed%8d")
  }


  // a little helper
  private[this] def createNewCache[K,V](size:Long, ttlMillis: Long):Cache[K, V] = {
    MapCache.createCache(new NodeCacheConfig {
      override val maxSize: Long = size
      override val maxHoldTime: FiniteDuration = FiniteDuration.apply(ttlMillis, TimeUnit.MILLISECONDS)
    })
  }

  private[this] def createOldCache[K,V](size:Long, ttlMillis: Long):Cache[K, V] = {
    OldCache.createCache(new NodeCacheConfig {
      override val maxSize: Long = size
      override val maxHoldTime: FiniteDuration = FiniteDuration.apply(ttlMillis, TimeUnit.MILLISECONDS)
    })
  }

}
