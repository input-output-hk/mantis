package io.iohk.ethereum.blockchain.sync.fast

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class HeaderSkeletonSpec extends AnyWordSpec with Matchers {

  "HeaderSkeleton" should {
    "calculate its properties correctly" in {
      val skeleton = HeaderSkeleton(from = 11, to = 20, maxSkeletonHeaders = 4)
      assert(skeleton.batchSize === BigInt(4))
      assert(skeleton.gapSize === BigInt(3))
      assert(skeleton.firstSkeletonHeaderNumber === BigInt(14)) // 11, 12, 13, [14] <- last header of first batch
      assert(skeleton.lastSkeletonHeaderNumber === BigInt(18)) // 11, 12, 13, [14], 15, 16, 17, [18], 19, 20
      assert(
        skeleton.limit === BigInt(2)
      ) // 11, 12, 13, [14], 15, 16, 17, [18], 19, 20 <- limit == number of skeleton headers in header skeleton
    }
    "calculate its properties correctly when headers in range don't fit into single skeleton" in {
      // maxSkeletonHeaders is defined by blockHeadersPerRequest which is also the maximum num for our batch size
      // so the maximum number we can fit into one skeleton (given that we only make one request per batch) is maxSkeletonHeaders * maxSkeletonHeaders
      val skeleton = HeaderSkeleton(from = 11, to = 20, maxSkeletonHeaders = 4)
      assert(skeleton.batchSize === BigInt(4))
      assert(skeleton.gapSize === BigInt(3))
      assert(skeleton.firstSkeletonHeaderNumber === BigInt(14)) // 11, 12, 13, [14] <- last header of first batch
      assert(skeleton.lastSkeletonHeaderNumber === BigInt(18)) // 11, 12, 13, [14], 15, 16, 17, [18], 19, 20
      assert(
        skeleton.limit === BigInt(2)
      ) // 11, 12, 13, [14], 15, 16, 17, [18], 19, 20 <- limit == number of skeleton headers in header skeleton
    }
    "calculate its properties correctly when num of remaining headers is smaller than maxSkeletonHeaders" in {
      val skeleton = HeaderSkeleton(from = 11, to = 20, maxSkeletonHeaders = 100)
      assert(skeleton.batchSize === BigInt(10))
      assert(skeleton.gapSize === BigInt(9))
      assert(
        skeleton.firstSkeletonHeaderNumber === BigInt(20)
      ) // 11, 12, 13, 14, 15, 16, 17, 18, 19, [20] <- last header of first batch
      assert(
        skeleton.lastSkeletonHeaderNumber === skeleton.firstSkeletonHeaderNumber
      ) // 11, 12, 13, 14, 15, 16, 17, 18 19, [20]
      assert(
        skeleton.limit === BigInt(1)
      ) // 11, 12, 13, 14, 15, 16, 17, 18, 19, [20] <- limit == number of skeleton headers in header skeleton
    }
  }

}
