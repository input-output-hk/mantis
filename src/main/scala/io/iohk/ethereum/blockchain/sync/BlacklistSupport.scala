package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.{Actor, ActorLogging, Cancellable, Scheduler}
import io.iohk.ethereum.network.PeerId

trait BlacklistSupport {
  selfActor: Actor with ActorLogging =>

  import BlacklistSupport._

  def scheduler: Scheduler

  var blacklistedPeers: Seq[(PeerId, Cancellable)] = Nil

  def blacklist(peerId: PeerId, duration: FiniteDuration, reason: String): Unit = {
    undoBlacklist(peerId)
    log.debug(s"Blacklisting peer ($peerId), $reason")
    val unblacklistCancellable = scheduler.scheduleOnce(duration, self, UnblacklistPeer(peerId))
    blacklistedPeers :+= (peerId, unblacklistCancellable)
  }

  def undoBlacklist(peerId: PeerId): Unit = {
    blacklistedPeers.find(_._1 == peerId).foreach(_._2.cancel())
    blacklistedPeers = blacklistedPeers.filterNot(_._1 == peerId)
  }

  def isBlacklisted(peerId: PeerId): Boolean =
    blacklistedPeers.exists(_._1 == peerId)
}

object BlacklistSupport {
  case class BlacklistPeer(peerId: PeerId, reason: String)
  case class UnblacklistPeer(peerId: PeerId)
}
