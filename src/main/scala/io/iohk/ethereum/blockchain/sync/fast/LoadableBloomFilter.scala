package io.iohk.ethereum.blockchain.sync.fast

import com.google.common.hash.{BloomFilter, Funnel}
import io.iohk.ethereum.blockchain.sync.fast.LoadableBloomFilter.BloomFilterLoadingResult
import io.iohk.ethereum.db.dataSource.RocksDbDataSource.IterationError
import monix.eval.Task
import monix.reactive.{Consumer, Observable}

class LoadableBloomFilter[A](bloomFilter: BloomFilter[A], source: Observable[Either[IterationError, A]]) {
  val loadFromSource: Task[BloomFilterLoadingResult] = {
    source
      .consumeWith(Consumer.foldLeftTask(BloomFilterLoadingResult()) { (s, e) =>
        e match {
          case Left(value) => Task.now(s.copy(error = Some(value)))
          case Right(value) => Task(bloomFilter.put(value)).map(_ => s.copy(writtenElements = s.writtenElements + 1))
        }
      })
      .memoizeOnSuccess
  }

  def put(elem: A): Boolean = bloomFilter.put(elem)

  def mightContain(elem: A): Boolean = bloomFilter.mightContain(elem)

  def approximateElementCount: Long = bloomFilter.approximateElementCount()
}

object LoadableBloomFilter {
  def apply[A](expectedSize: Int, loadingSource: Observable[Either[IterationError, A]])(implicit
      f: Funnel[A]
  ): LoadableBloomFilter[A] = {
    new LoadableBloomFilter[A](BloomFilter.create[A](f, expectedSize), loadingSource)
  }

  case class BloomFilterLoadingResult(writtenElements: Long, error: Option[IterationError])
  object BloomFilterLoadingResult {
    def apply(): BloomFilterLoadingResult = new BloomFilterLoadingResult(0, None)

    def apply(ex: Throwable): BloomFilterLoadingResult = new BloomFilterLoadingResult(0, Some(IterationError(ex)))
  }
}
