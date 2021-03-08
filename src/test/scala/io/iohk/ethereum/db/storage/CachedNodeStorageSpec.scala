package io.iohk.ethereum.db.storage

import java.util.concurrent.TimeUnit
import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.cache.MapCache
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.utils.Config.NodeCacheConfig
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.concurrent.duration._
import scala.concurrent.duration.FiniteDuration
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CachedNodeStorageSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with ObjectGenerators {
  val iterations = 10

  "CachedNodeStorage" should "not update dataSource until persist" in new TestSetup {
    forAll(keyValueByteStringGen(kvSize)) { keyvalues =>
      cachedNodeStorage.update(Nil, keyvalues)
    }
    dataSource.storage shouldBe empty

    val cachedValuesSize = cachedNodeStorage.cache.getValues.size

    if (cachedNodeStorage.persist()) {
      cachedNodeStorage.cache.getValues shouldBe empty
      dataSource.storage.size shouldEqual cachedValuesSize
    } else {
      dataSource.storage shouldBe empty
    }
  }

  it should "persist elements to underlying data source when full" in new TestSetup {
    forAll(keyValueByteStringGen(kvSize)) { keyvalues =>
      cachedNodeStorage.update(Nil, keyvalues)

      if (underLying.size > testCapacityCacheConfig.maxSize)
        assert(cachedNodeStorage.persist())

      keyvalues.foreach(elem => assert(cachedNodeStorage.get(elem._1).get.sameElements(elem._2)))
    }
  }

  it should "persist elements to underlying data source when not cleared for long time" in new TestSetup {
    val key = ByteString(1)
    val value = Array(1.toByte)
    val cachedNodeStorageTiming = new CachedNodeStorage(nodeStorage, mapCacheTime)
    cachedNodeStorageTiming.update(Nil, Seq((key, value)))
    Thread.sleep(1.second.toMillis)
    cachedNodeStorageTiming.persist() shouldEqual true
    dataSource.storage.nonEmpty shouldBe true
  }

  trait TestSetup {
    val dataSource = EphemDataSource()
    val nodeStorage = new NodeStorage(dataSource)
    val underLying = MapCache.getMap[NodeHash, NodeEncoded]
    val mapCache: MapCache[NodeHash, NodeEncoded] =
      new MapCache[NodeHash, NodeEncoded](underLying, testCapacityCacheConfig)
    val mapCacheTime: MapCache[NodeHash, NodeEncoded] =
      new MapCache[NodeHash, NodeEncoded](underLying, testTimeCacheConfig)
    val cachedNodeStorage = new CachedNodeStorage(nodeStorage, mapCache)

    val kvSize = 64

    object testCapacityCacheConfig extends NodeCacheConfig {
      override val maxSize = 30
      override val maxHoldTime = FiniteDuration(10, TimeUnit.MINUTES)
    }

    object testTimeCacheConfig extends NodeCacheConfig {
      override val maxSize = 30
      override val maxHoldTime = FiniteDuration(1, TimeUnit.SECONDS)
    }

  }
}
