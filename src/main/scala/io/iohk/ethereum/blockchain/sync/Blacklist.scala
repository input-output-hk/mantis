package io.iohk.ethereum.blockchain.sync

import com.github.blemale.scaffeine.{Cache, Scaffeine}
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.jdk.DurationConverters._
import Blacklist._
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.BlockError
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason.BlacklistReasonType
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect

trait Blacklist {
  def isBlacklisted(id: BlacklistId): Boolean
  def add(id: BlacklistId, duration: FiniteDuration, reason: BlacklistReason): Unit
  def remove(id: BlacklistId): Unit
  def keys: Set[BlacklistId]
}

// scalastyle:off number.of.types number.of.methods
object Blacklist {
  import BlacklistReason._
  import BlacklistReasonType._
  import BlacklistGroup._

  trait BlacklistId {
    def value: String
  }

  sealed trait BlacklistReason {
    def reasonType: BlacklistReasonType
    def description: String
  }

  object BlacklistGroup {
    trait FastSyncBlacklistGroup {
      val group = "FastSyncBlacklistGroup"
    }
    trait RegularSyncBlacklistGroup {
      val group = "RegularSyncBlacklistGroup"
    }
    //this group is directly translated from WireProtocol
    trait P2PBlacklistGroup {
      val group = "P2PBlacklistGroup"
    }
  }

  object BlacklistReason {
    sealed trait BlacklistReasonType {
      def code: Int
      def name: String
      def group: String
    }
    object BlacklistReasonType {
      case object WrongBlockHeadersType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 1
        val name: String = "WrongBlockHeadersType"
      }
      case object BlockHeaderValidationFailedType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 2
        val name: String = "BlockHeaderValidationFailed"
      }
      case object ErrorInBlockHeadersType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 3
        val name: String = "ErrorInBlockHeaders"
      }
      case object EmptyBlockBodiesType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 4
        val name: String = "EmptyBlockBodies"
      }
      case object BlockBodiesNotMatchingHeadersType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 5
        val name: String = "BlockBodiesNotMatchingHeaders"
      }
      case object EmptyReceiptsType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 6
        val name: String = "EmptyReceipts"
      }
      case object InvalidReceiptsType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 7
        val name: String = "InvalidReceipts"
      }
      case object FastSyncRequestFailedType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 8
        val name: String = "FastSyncRequestFailed"
      }
      case object PeerActorTerminatedType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 9
        val name: String = "PeerActorTerminated"
      }
      case object InvalidStateResponseType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 10
        val name: String = "InvalidStateResponse"
      }
      case object InvalidPivotBlockElectionResponseType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 11
        val name: String = "InvalidPivotElectionResponse"
      }
      case object PivotBlockElectionTimeoutType extends BlacklistReasonType with FastSyncBlacklistGroup {
        val code: Int = 12
        val name: String = "PivotBlockElectionTimeout"
      }
      case object DisconnectRequestedType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 13
        val name: String = "DisconnectRequested"
      }
      case object TcpSubsystemErrorType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 14
        val name: String = "TcpSubsystemError"
      }
      case object UselessPeerType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 15
        val name: String = "UselessPeer"
      }
      case object TooManyPeersType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 16
        val name: String = "TooManyPeers"
      }
      case object AlreadyConnectedType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 17
        val name: String = "AlreadyConnected"
      }
      case object IncompatibleP2pProtocolVersionType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 18
        val name: String = "IncompatibleP2pProtocolVersion"
      }
      case object NullNodeIdentityReceivedType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 19
        val name: String = "NullNodeIdentityReceived"
      }
      case object ClientQuittingType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 20
        val name: String = "ClientQuitting"
      }
      case object UnexpectedIdentityType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 21
        val name: String = "UnexpectedIdentity"
      }
      case object IdentityTheSameType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 22
        val name: String = "IdentityTheSame"
      }
      case object TimeoutOnReceivingAMessageType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 23
        val name: String = "TimeoutOnReceivingAMessage"
      }
      case object OtherSubprotocolSpecificReasonType extends BlacklistReasonType with P2PBlacklistGroup {
        val code: Int = 24
        val name: String = "OtherSubprotocolSpecificReason"
      }
      case object EmptyStateNodeResponseType extends BlacklistReasonType with RegularSyncBlacklistGroup {
        val code: Int = 25
        val name: String = "EmptyStateNodeResponse"
      }
      case object WrongStateNodeResponseType extends BlacklistReasonType with RegularSyncBlacklistGroup {
        val code: Int = 26
        val name: String = "WrongStateNodeResponse"
      }
      case object UnrequestedBodiesType extends BlacklistReasonType with RegularSyncBlacklistGroup {
        val code: Int = 27
        val name: String = "UnrequestedBodies"
      }
      case object UnrequestedHeadersType extends BlacklistReasonType with RegularSyncBlacklistGroup {
        val code: Int = 28
        val name: String = "UnrequestedHeaders"
      }
      case object RegularSyncRequestFailedType extends BlacklistReasonType with RegularSyncBlacklistGroup {
        val code: Int = 29
        val name: String = "RegularSyncRequestFailed"
      }
      case object BlockImportErrorType extends BlacklistReasonType with RegularSyncBlacklistGroup {
        val code: Int = 30
        val name: String = "BlockImportError"
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
    final case class FastSyncRequestFailed(error: String) extends BlacklistReason {
      val reasonType: BlacklistReasonType = FastSyncRequestFailedType
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
    case object DisconnectRequested extends BlacklistReason {
      val reasonType: BlacklistReasonType = DisconnectRequestedType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.DisconnectRequested)
    }
    case object TcpSubsystemError extends BlacklistReason {
      val reasonType: BlacklistReasonType = TcpSubsystemErrorType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.TcpSubsystemError)
    }
    case object UselessPeer extends BlacklistReason {
      val reasonType: BlacklistReasonType = UselessPeerType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.UselessPeer)
    }
    case object TooManyPeers extends BlacklistReason {
      val reasonType: BlacklistReasonType = TooManyPeersType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.TooManyPeers)
    }
    case object AlreadyConnected extends BlacklistReason {
      val reasonType: BlacklistReasonType = AlreadyConnectedType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.AlreadyConnected)
    }
    case object IncompatibleP2pProtocolVersion extends BlacklistReason {
      val reasonType: BlacklistReasonType = IncompatibleP2pProtocolVersionType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.IncompatibleP2pProtocolVersion)
    }
    case object NullNodeIdentityReceived extends BlacklistReason {
      val reasonType: BlacklistReasonType = NullNodeIdentityReceivedType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.NullNodeIdentityReceived)
    }
    case object ClientQuitting extends BlacklistReason {
      val reasonType: BlacklistReasonType = ClientQuittingType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.ClientQuitting)
    }
    case object UnexpectedIdentity extends BlacklistReason {
      val reasonType: BlacklistReasonType = UnexpectedIdentityType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.UnexpectedIdentity)
    }
    case object IdentityTheSame extends BlacklistReason {
      val reasonType: BlacklistReasonType = IdentityTheSameType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.IdentityTheSame)
    }
    case object TimeoutOnReceivingAMessage extends BlacklistReason {
      val reasonType: BlacklistReasonType = TimeoutOnReceivingAMessageType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.TimeoutOnReceivingAMessage)
    }
    case object OtherSubprotocolSpecificReason extends BlacklistReason {
      val reasonType: BlacklistReasonType = OtherSubprotocolSpecificReasonType
      val description: String = Disconnect.reasonToString(Disconnect.Reasons.Other)
    }
    case object EmptyStateNodeResponse extends BlacklistReason {
      val reasonType: BlacklistReasonType = EmptyStateNodeResponseType
      val description: String = "Empty state node response from peer"
    }
    case object WrongStateNodeResponse extends BlacklistReason {
      val reasonType: BlacklistReasonType = WrongStateNodeResponseType
      val description: String = "Fetched node state hash doesn't match requested one"
    }
    case object UnrequestedBodies extends BlacklistReason {
      val reasonType: BlacklistReasonType = UnrequestedBodiesType
      val description: String = "Received unrequested bodies"
    }
    case object UnrequestedHeaders extends BlacklistReason {
      val reasonType: BlacklistReasonType = UnrequestedHeadersType
      val description: String = "Received unrequested headers"
    }
    final case class RegularSyncRequestFailed(error: String) extends BlacklistReason {
      val reasonType: BlacklistReasonType = RegularSyncRequestFailedType
      val description: String = s"Request failed with error: $error"
    }
    final case class BlockImportError(error: String) extends BlacklistReason {
      val reasonType: BlacklistReasonType = BlockImportErrorType
      val description: String = s"Block import error: $error"
    }

    private val allP2PReasons = List(
      DisconnectRequested,
      TcpSubsystemError,
      UselessPeer,
      TooManyPeers,
      AlreadyConnected,
      IncompatibleP2pProtocolVersion,
      NullNodeIdentityReceived,
      ClientQuitting,
      UnexpectedIdentity,
      IdentityTheSame,
      TimeoutOnReceivingAMessage,
      OtherSubprotocolSpecificReason
    )

    def getP2PBlacklistReasonByDescription(description: String): BlacklistReason = {
      allP2PReasons.find(_.description == description).getOrElse(OtherSubprotocolSpecificReason)
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
