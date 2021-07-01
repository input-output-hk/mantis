package io.iohk.ethereum.blockchain.sync

import java.util.concurrent.TimeUnit

import scala.concurrent.duration._

import com.github.blemale.scaffeine.Scaffeine
import com.google.common.testing.FakeTicker
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import io.iohk.ethereum.network.PeerId

class CacheBasedBlacklistSpec extends AnyWordSpecLike with Matchers {
  import Blacklist._

  private val peer1 = PeerId("1")
  private val peer2 = PeerId("2")
  private val peer3 = PeerId("3")
  private val peer4 = PeerId("4")
  private val peer5 = PeerId("5")

  private val reason = BlacklistReason.ErrorInBlockHeaders
  private val anotherReason = BlacklistReason.BlockBodiesNotMatchingHeaders

  private def withBlacklist(maxElements: Int)(test: CacheBasedBlacklist => Unit): Unit = {
    val blacklist = CacheBasedBlacklist.empty(maxElements)
    test(blacklist)
  }

  "CacheBasedBlacklist" should {
    "add elements and respect max number of elements" in withBlacklist(3) { blacklist =>
      blacklist.add(peer1, 1.minute, reason)
      blacklist.add(peer2, 1.minute, reason)
      blacklist.add(peer3, 1.minute, anotherReason)
      blacklist.add(peer4, 1.minute, anotherReason)
      blacklist.add(peer5, 1.minute, reason)
      blacklist.cache.cleanUp()
      val size = blacklist.keys.size
      assert(size <= 3 && size > 0)
    }
    "should expire elements" in {
      val maxSize = 10
      val ticker = new FakeTicker()
      val cache = Scaffeine()
        .expireAfter[BlacklistId, BlacklistReason.BlacklistReasonType](
          create = (_, _) => 60.minutes,
          update = (_, _, _) => 60.minutes,
          read = (_, _, duration) => duration
        )
        .maximumSize(
          maxSize
        )
        .ticker(ticker.read _)
        .build[BlacklistId, BlacklistReason.BlacklistReasonType]()
      val blacklist = CacheBasedBlacklist(cache)
      blacklist.add(peer1, 1.minute, reason)
      blacklist.add(peer2, 10.minutes, reason)
      blacklist.add(peer3, 3.minutes, anotherReason)
      blacklist.add(peer4, 2.minutes, reason)
      blacklist.add(peer5, 7.minutes, reason)
      blacklist.isBlacklisted(peer2) // just to simulate a read
      blacklist.keys // just to simulate a read
      ticker.advance(5, TimeUnit.MINUTES)
      val expected = Set(peer2, peer5)
      blacklist.cache.cleanUp()
      blacklist.keys must contain theSameElementsAs expected
    }
    "check if given key is part of the list" in withBlacklist(3) { blacklist =>
      blacklist.add(peer1, 1.minute, reason)
      blacklist.add(peer2, 1.minute, anotherReason)
      blacklist.add(peer3, 1.minute, reason)
      assert(blacklist.isBlacklisted(peer2) === true)
      assert(blacklist.isBlacklisted(PeerId("7")) === false)
    }
    "remove id from blacklist" in withBlacklist(3) { blacklist =>
      blacklist.add(peer1, 1.minute, reason)
      blacklist.add(peer2, 1.minute, anotherReason)
      blacklist.add(peer3, 1.minute, reason)
      assert(blacklist.isBlacklisted(peer2) === true)
      blacklist.remove(peer2)
      assert(blacklist.isBlacklisted(peer2) === false)
    }
  }

}
