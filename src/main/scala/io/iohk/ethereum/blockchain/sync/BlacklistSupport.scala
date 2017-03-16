package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Scheduler}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration

trait BlacklistSupport {
  selfActor: Actor with ActorLogging =>

  import BlacklistSupport._

  def scheduler: Scheduler

  var blacklistedPeers: Seq[BlacklistedPeer] = Nil

  def blacklist(peer: ActorRef, duration: FiniteDuration, reason: String): Unit = {
    undoBlacklist(peer)
    val unblacklistCancellable = scheduler.scheduleOnce(duration, self, UnblacklistPeer(peer))
    log.info(s"Blacklisting peer. Reason: $reason")
    blacklistedPeers :+= BlacklistedPeer(peer, unblacklistCancellable, reason)
  }

  def undoBlacklist(peer: ActorRef): Unit = {
    blacklistedPeers.find(_.peer == peer).foreach {
      case BlacklistedPeer(_, cancellable, reason) =>
        log.info(s"Removing peer from blacklist. Original reason was: $reason")
        cancellable.cancel()
    }
    blacklistedPeers = blacklistedPeers.filterNot(_.peer == peer)
  }

  def isBlacklisted(peer: ActorRef): Boolean =
    blacklistedPeers.exists(_.peer == peer)
}

object BlacklistSupport {

  case class BlacklistPeer(peer: ActorRef, reason: String)
  case class UnblacklistPeer(peer: ActorRef)

  case class BlacklistedPeer(peer: ActorRef, cancellable: Cancellable, reason: String)
}
