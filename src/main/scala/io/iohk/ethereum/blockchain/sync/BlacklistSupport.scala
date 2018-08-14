package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration.{Duration, FiniteDuration}
import akka.actor.{Actor, ActorLogging, Cancellable, Scheduler}
import io.iohk.ethereum.network.PeerId

import scala.concurrent.ExecutionContext.Implicits.global

trait BlacklistSupport {
  selfActor: Actor with ActorLogging =>

  import BlacklistSupport._

  def scheduler: Scheduler

  var blacklistedPeers: Seq[(PeerId, Cancellable)] = Nil

  def blacklist(peerId: PeerId, duration: FiniteDuration, reason: String): Unit = {
    if (duration > Duration.Zero) {
      undoBlacklist(peerId)
      log.info(s"Blacklisting peer ($peerId), $reason")
      val unblacklistCancellable = scheduler.scheduleOnce(duration, self, UnblacklistPeer(peerId))
      blacklistedPeers :+= (peerId, unblacklistCancellable)
    } else {
      log.info(s"Peer ($peerId) would be blacklisted (reason: $reason), but blacklisting duration is zero [= $duration]")
    }
  }

  def undoBlacklist(peerId: PeerId): Unit = {
    blacklistedPeers.find(_._1 == peerId).foreach(_._2.cancel())
    blacklistedPeers = blacklistedPeers.filterNot(_._1 == peerId)
  }

  def isBlacklisted(peerId: PeerId): Boolean =
    blacklistedPeers.exists(_._1 == peerId)

  def handleBlacklistMessages: Receive = {
    case UnblacklistPeer(ref) => undoBlacklist(ref)
  }
}

object BlacklistSupport {
  private case class UnblacklistPeer(peerId: PeerId)
}
