package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, Cancellable, Scheduler}
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistId
import io.iohk.ethereum.utils.Logger

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, FiniteDuration}

// will be removed once regular sync is switched to new blacklist implementation
trait BlacklistSupport {
  selfActor: Actor with Logger =>

  import BlacklistSupport._

  def scheduler: Scheduler

  protected val maxBlacklistedNodes = 1000

  val blacklistedPeers = mutable.LinkedHashMap.empty[BlacklistId, Cancellable]

  def blacklist(blacklistId: BlacklistId, duration: FiniteDuration, reason: String): Unit = {
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

  def undoBlacklist(blacklistId: BlacklistId): Unit = {
    val peer = blacklistedPeers.get(blacklistId)
    peer.foreach(_.cancel())
    blacklistedPeers.remove(blacklistId)
  }

  def isBlacklisted(blacklistId: BlacklistId): Boolean =
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

  private case class UnblacklistPeer(blacklistId: BlacklistId)

}
