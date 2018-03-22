package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration.FiniteDuration
import akka.actor.{Actor, ActorLogging, Cancellable, Scheduler}

import scala.concurrent.ExecutionContext.Implicits.global

trait BlacklistSupport {
  selfActor: Actor with ActorLogging =>

  import BlacklistSupport._

  def scheduler: Scheduler

  private val maxSize = 1000

  var blacklistedPeers: Seq[(BlackListId, Cancellable)] = Nil

  def blacklist(peerId: BlackListId, duration: FiniteDuration, reason: String): Unit = {

    if (blacklistedPeers.size >= maxSize) {
      removeOldestPeer()
    }
    undoBlacklist(peerId)
    log.debug(s"Blacklisting peer ($peerId), $reason")
    val unblacklistCancellable = scheduler.scheduleOnce(duration, self, UnblacklistPeer(peerId))
    blacklistedPeers :+= (peerId, unblacklistCancellable)
  }

  def undoBlacklist(peerId: BlackListId): Unit = {
    blacklistedPeers.find(_._1 == peerId).foreach(_._2.cancel())
    blacklistedPeers = blacklistedPeers.filterNot(_._1 == peerId)
  }

  def isBlacklisted(peerId: BlackListId): Boolean =
    blacklistedPeers.exists(_._1 == peerId)

  def handleBlacklistMessages: Receive = {
    case UnblacklistPeer(ref) => undoBlacklist(ref)
  }

  private def removeOldestPeer(): Unit = {
    val oldestPeer = blacklistedPeers.head
    oldestPeer._2.cancel()
    blacklistedPeers = blacklistedPeers.tail
  }
}

object BlacklistSupport {

  abstract class BlackListId {
    def value: String
  }

  private case class UnblacklistPeer(peerId: BlackListId)
}
