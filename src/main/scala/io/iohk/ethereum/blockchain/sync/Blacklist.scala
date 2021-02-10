package io.iohk.ethereum.blockchain.sync

import com.github.blemale.scaffeine.{Cache, Scaffeine}
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.FiniteDuration

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.jdk.DurationConverters._

trait Blacklist {
  def isBlacklisted(id: BlackListId): Boolean
  def add(id: BlackListId, duration: FiniteDuration, reason: String): Unit
  def remove(id: BlackListId): Unit
  def keys: Set[BlackListId]
}

final case class CacheBasedBlacklist(cache: Cache[BlackListId, String])
    extends Blacklist with Logger {

  override def isBlacklisted(id: BlackListId): Boolean = cache.getIfPresent(id).isDefined
  override def add(id: BlackListId, duration: FiniteDuration, reason: String): Unit =
    cache.policy().expireVariably().toScala.fold {
      log.warn(s"Unexpected error while adding peer [$id] to blacklist using custom expiration time. Falling back to default expiration.")
      cache.put(id, reason)
    } { ve =>
      ve.put(id, reason, duration.toJava)
    }
  override def remove(id: BlackListId): Unit = cache.invalidate(id)

  override def keys: Set[BlackListId] = cache.underlying.asMap().keySet().asScala.toSet
}

object CacheBasedBlacklist {

  // TODO check eviction strategy - we want to remove the oldest element if maxSize is reached
  def empty(maxSize: Int): CacheBasedBlacklist = {
    val cache =
      Scaffeine()
        .maximumSize(maxSize)
        .build[BlackListId, String]()
    CacheBasedBlacklist(cache)
  }

}
