package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration.FiniteDuration
import akka.actor.{Actor, ActorLogging, Cancellable, Scheduler}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

trait BlacklistSupport {
  selfActor: Actor with ActorLogging =>

  import BlacklistSupport._

  def scheduler: Scheduler

  private val maxSize = 1000

  val blacklistedPeers = mutable.LinkedHashMap.empty[BlackListId, Cancellable]

  def blacklist(peerId: BlackListId, duration: FiniteDuration, reason: String): Unit = {

    if (blacklistedPeers.size >= maxSize) {
      removeOldestPeer()
    }
    undoBlacklist(peerId)
    log.debug(s"Blacklisting peer ($peerId), $reason")
    val unblacklistCancellable = scheduler.scheduleOnce(duration, self, UnblacklistPeer(peerId))
    blacklistedPeers.put(peerId, unblacklistCancellable)
  }

  def undoBlacklist(peerId: BlackListId): Unit = {
    val peer = blacklistedPeers.get(peerId)
    peer.foreach(_.cancel())
    blacklistedPeers.remove(peerId)
  }

  def isBlacklisted(peerId: BlackListId): Boolean =
    blacklistedPeers.exists(_._1 == peerId)

  def handleBlacklistMessages: Receive = {
    case UnblacklistPeer(ref) => undoBlacklist(ref)
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

  private case class UnblacklistPeer(peerId: BlackListId)
}
