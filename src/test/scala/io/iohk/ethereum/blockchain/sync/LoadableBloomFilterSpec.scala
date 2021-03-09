package io.iohk.ethereum.blockchain.sync

import com.google.common.hash.Funnel
import com.google.common.hash.Funnels
import com.google.common.hash.PrimitiveSink
import io.iohk.ethereum.FlatSpecBase
import io.iohk.ethereum.blockchain.sync.fast.LoadableBloomFilter
import io.iohk.ethereum.db.dataSource.RocksDbDataSource.IterationError
import monix.eval.Task
import monix.reactive.Observable

class LoadableBloomFilterSpec extends FlatSpecBase {
  implicit object LongFun extends Funnel[Long] {
    override def funnel(from: Long, into: PrimitiveSink): Unit =
      Funnels.longFunnel().funnel(from, into)
  }

  "LoadableBloomFilter" should "load all correct elements " in testCaseM {
    for {
      source <- Task(Observable.fromIterable(Seq(Right(1L), Right(2L), Right(3L))))
      filter = LoadableBloomFilter[Long](1000, source)
      result <- filter.loadFromSource
    } yield {
      assert(result.writtenElements == 3)
      assert(result.error.isEmpty)
      assert(filter.approximateElementCount == 3)
    }
  }

  it should "load filter only once" in testCaseM[Task] {
    for {
      source <- Task(Observable.fromIterable(Seq(Right(1L), Right(2L), Right(3L))))
      filter = LoadableBloomFilter[Long](1000, source)
      result <- filter.loadFromSource
      result1 <- filter.loadFromSource
    } yield {
      assert(result.writtenElements == 3)
      assert(result.error.isEmpty)
      assert(filter.approximateElementCount == 3)
      assert(result1 == result)
    }
  }

  it should "report last error if encountered" in testCaseM[Task] {
    for {
      error <- Task(IterationError(new RuntimeException("test")))
      source = Observable.fromIterable(Seq(Right(1L), Right(2L), Right(3L), Left(error)))
      filter = LoadableBloomFilter[Long](1000, source)
      result <- filter.loadFromSource
    } yield {
      assert(result.writtenElements == 3)
      assert(result.error.contains(error))
      assert(filter.approximateElementCount == 3)
    }
  }

}
