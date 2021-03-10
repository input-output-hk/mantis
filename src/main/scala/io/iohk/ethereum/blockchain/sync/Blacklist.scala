package io.iohk.ethereum.blockchain.sync

import com.github.benmanes.caffeine.cache.Caffeine
import com.github.blemale.scaffeine.{Cache, Scaffeine}
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.jdk.DurationConverters._

import Blacklist._
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason.BlacklistReasonType.WrongBlockHeadersType
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.BlockError
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason.BlacklistReasonType

trait Blacklist {
  def isBlacklisted(id: BlacklistId): Boolean
  def add(id: BlacklistId, duration: FiniteDuration, reason: BlacklistReason): Unit
  def remove(id: BlacklistId): Unit
  def keys: Set[BlacklistId]
}

object Blacklist {
  import BlacklistReason._
  import BlacklistReasonType._

  trait BlacklistId {
    def value: String
  }

  sealed trait BlacklistReason {
    def reasonType: BlacklistReasonType
    def description: String
  }
  object BlacklistReason {
    sealed trait BlacklistReasonType {
      def code: Int
      def name: String
    }
    object BlacklistReasonType {
      case object WrongBlockHeadersType extends BlacklistReasonType {
        val code: Int = 1
        val name: String = "WrongBlockHeadersType"
      }
      case object BlockHeaderValidationFailedType extends BlacklistReasonType {
        val code: Int = 2
        val name: String = "BlockHeaderValidationFailed"
      }
      case object ErrorInBlockHeadersType extends BlacklistReasonType {
        val code: Int = 3
        val name: String = "ErrorInBlockHeaders"
      }
      case object EmptyBlockBodiesType extends BlacklistReasonType {
        val code: Int = 4
        val name: String = "EmptyBlockBodies"
      }
      case object BlockBodiesNotMatchingHeadersType extends BlacklistReasonType {
        val code: Int = 5
        val name: String = "BlockBodiesNotMatchingHeaders"
      }
      case object EmptyReceiptsType extends BlacklistReasonType {
        val code: Int = 6
        val name: String = "EmptyReceipts"
      }
      case object InvalidReceiptsType extends BlacklistReasonType {
        val code: Int = 7
        val name: String = "InvalidReceipts"
      }
      case object RequestFailedType extends BlacklistReasonType {
        val code: Int = 8
        val name: String = "RequestFailed"
      }
      case object PeerActorTerminatedType extends BlacklistReasonType {
        val code: Int = 9
        val name: String = "PeerActorTerminated"
      }
      case object InvalidStateResponseType extends BlacklistReasonType {
        val code: Int = 10
        val name: String = "InvalidStateResponse"
      }
      case object InvalidPivotBlockElectionResponseType extends BlacklistReasonType {
        val code: Int = 11
        val name: String = "InvalidPivotElectionResponse"
      }
      case object PivotBlockElectionTimeoutType extends BlacklistReasonType {
        val code: Int = 12
        val name: String = "PivotBlockElectionTimeout"
      }
    }

    case object WrongBlockHeaders extends BlacklistReason {
      val reasonType: BlacklistReasonType = WrongBlockHeadersType
      val description: String = "Wrong blockheaders response: Peer didn't respond with requested block headers."
    }
    case object BlockHeaderValidationFailed extends BlacklistReason {
      val reasonType: BlacklistReasonType = BlockHeaderValidationFailedType
      val description: String = "Block header validation failed"
    }
    case object ErrorInBlockHeaders extends BlacklistReason {
      val reasonType: BlacklistReasonType = ErrorInBlockHeadersType
      val description: String = "Error in block headers response"
    }
    final case class EmptyBlockBodies(knownHashes: Seq[String]) extends BlacklistReason {
      val reasonType: BlacklistReasonType = EmptyBlockBodiesType
      val description: String = s"Got empty block bodies response for known hashes: $knownHashes"
    }
    case object BlockBodiesNotMatchingHeaders extends BlacklistReason {
      val reasonType: BlacklistReasonType = BlockBodiesNotMatchingHeadersType
      val description = "Block bodies not matching block headers"
    }
    final case class EmptyReceipts(knownHashes: Seq[String]) extends BlacklistReason {
      val reasonType: BlacklistReasonType = EmptyReceiptsType
      val description: String = s"Got empty receipts for known hashes: $knownHashes"
    }
    final case class InvalidReceipts(knownHashes: Seq[String], error: BlockError) extends BlacklistReason {
      val reasonType: BlacklistReasonType = InvalidReceiptsType
      val description: String = s"Got invalid receipts for known hashes: $knownHashes due to: $error"
    }
    final case class RequestFailed(error: String) extends BlacklistReason {
      val reasonType: BlacklistReasonType = RequestFailedType
      val description: String = s"Request failed with error: $error"
    }
    case object PeerActorTerminated extends BlacklistReason {
      val reasonType: BlacklistReasonType = PeerActorTerminatedType
      val description: String = "Peer actor terminated"
    }
    final case class InvalidStateResponse(details: String) extends BlacklistReason {
      val reasonType: BlacklistReasonType = InvalidStateResponseType
      val description: String = s"Invalid response while syncing state trie: $details"
    }
    case object InvalidPivotBlockElectionResponse extends BlacklistReason {
      val reasonType: BlacklistReasonType = InvalidStateResponseType
      val description: String = "Invalid response while selecting pivot block"
    }
    case object PivotBlockElectionTimeout extends BlacklistReason {
      val reasonType: BlacklistReasonType = InvalidStateResponseType
      val description: String = "Peer didn't respond with requested pivot block candidate in a timely manner"
    }
  }
}

final case class CacheBasedBlacklist(cache: Cache[BlacklistId, BlacklistReasonType]) extends Blacklist with Logger {

  import CacheBasedBlacklist._

  override def isBlacklisted(id: BlacklistId): Boolean = cache.getIfPresent(id).isDefined

  override def add(id: BlacklistId, duration: FiniteDuration, reason: BlacklistReason): Unit = {
    log.info("Blacklisting peer [{}] for {}. Reason: {}", id, duration, reason.description)
    cache.policy().expireVariably().toScala match {
      case Some(varExpiration) => varExpiration.put(id, reason.reasonType, duration.toJava)
      case None =>
        log.warn(customExpirationError(id))
        cache.put(id, reason.reasonType)
    }
  }
  override def remove(id: BlacklistId): Unit = cache.invalidate(id)

  override def keys: Set[BlacklistId] = cache.underlying.asMap().keySet().asScala.toSet
}

object CacheBasedBlacklist {

  def customExpirationError(id: BlacklistId): String =
    s"Unexpected error while adding peer [${id.value}] to blacklist using custom expiration time. Falling back to default expiration."

  def empty(maxSize: Int): CacheBasedBlacklist = {
    val cache =
      Scaffeine()
        .expireAfter[BlacklistId, BlacklistReasonType](
          create = (_, _) => 60.minutes,
          update = (_, _, _) => 60.minutes,
          read = (_, _, duration) => duration // read access should not change the expiration time
        ) // required to enable VarExpiration policy (i.e. set custom expiration time per element)
        .maximumSize(
          maxSize
        ) // uses Window TinyLfu eviction policy, see https://github.com/ben-manes/caffeine/wiki/Efficiency
        .build[BlacklistId, BlacklistReasonType]()
    CacheBasedBlacklist(cache)
  }

}
