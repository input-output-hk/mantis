package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration.{Duration, FiniteDuration}
import akka.actor.{Actor, ActorLogging, Cancellable, Scheduler}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

trait BlacklistSupport {
  selfActor: Actor with ActorLogging =>

  import BlacklistSupport._

  def scheduler: Scheduler

  protected val maxBlacklistedNodes = 1000

  val blacklistedPeers = mutable.LinkedHashMap.empty[BlackListId, Cancellable]

  def blacklist(blacklistId: BlackListId, duration: FiniteDuration, reason: String): Unit = {
    if (duration > Duration.Zero) {
      if (blacklistedPeers.size >= maxBlacklistedNodes) {
        removeOldestPeer()
      }
      undoBlacklist(blacklistId)
      log.debug(s"Blacklisting peer ($blacklistId), $reason")
      val unblacklistCancellable = scheduler.scheduleOnce(duration, self, UnblacklistPeer(blacklistId))
      blacklistedPeers.put(blacklistId, unblacklistCancellable)
    } else {
      log.debug(s"Peer ($blacklistId) would be blacklisted (reason: $reason), but blacklisting duration is zero")
    }
  }

  def undoBlacklist(blacklistId: BlackListId): Unit = {
    val peer = blacklistedPeers.get(blacklistId)
    peer.foreach(_.cancel())
    blacklistedPeers.remove(blacklistId)
  }

  def isBlacklisted(blacklistId: BlackListId): Boolean =
    blacklistedPeers.exists(_._1 == blacklistId)

  def handleBlacklistMessages: Receive = { case UnblacklistPeer(ref) =>
    undoBlacklist(ref)
  }

  private def removeOldestPeer(): Unit = {
    val oldestPeer = blacklistedPeers.head
    oldestPeer._2.cancel()
    blacklistedPeers.remove(oldestPeer._1)
  }
}

object BlacklistSupport {

  abstract class BlackListId {
    def value: String
  }

  private case class UnblacklistPeer(blacklistId: BlackListId)
}
