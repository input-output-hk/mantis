package io.iohk.ethereum.blockchain.sync

import com.github.benmanes.caffeine.cache.Caffeine
import com.github.blemale.scaffeine.{Cache, Scaffeine}
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
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
      log.warn(s"Unexpected error while adding peer [${id.value}] to blacklist using custom expiration time. Falling back to default expiration.")
      cache.put(id, reason)
    } { varExpirationPolicy =>
      varExpirationPolicy.put(id, reason, duration.toJava)
    }
  override def remove(id: BlackListId): Unit = cache.invalidate(id)

  override def keys: Set[BlackListId] = cache.underlying.asMap().keySet().asScala.toSet
}

object CacheBasedBlacklist {

  def empty(maxSize: Int): CacheBasedBlacklist = {
    val cache =
      Scaffeine()
        .expireAfter[BlackListId, String](create = (_, _) => 60.minutes,
          update = (_, _, _) => 60.minutes,
          read = (_, _, _) => 60.minutes) // required to enable VarExpiration policy (i.e. set custom expiration time per element)
        .maximumSize(maxSize) // uses Window TinyLfu eviction policy, see https://github.com/ben-manes/caffeine/wiki/Efficiency
        .build[BlackListId, String]()
    CacheBasedBlacklist(cache)
  }

}
