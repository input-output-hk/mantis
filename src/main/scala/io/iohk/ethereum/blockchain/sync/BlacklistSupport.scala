package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Scheduler}

trait BlacklistSupport {
  selfActor: Actor with ActorLogging =>

  import BlacklistSupport._

  def scheduler: Scheduler

  var blacklistedPeers: Seq[(ActorRef, Cancellable)] = Nil

  def blacklist(peer: ActorRef, duration: FiniteDuration, reason: String): Unit = {
    undoBlacklist(peer)
    log.info(s"Blacklisting peer (${peer.path.name}), $reason")
    val unblacklistCancellable = scheduler.scheduleOnce(duration, self, UnblacklistPeer(peer))
    blacklistedPeers :+= (peer, unblacklistCancellable)
  }

  def undoBlacklist(peer: ActorRef): Unit = {
    blacklistedPeers.find(_._1 == peer).foreach(_._2.cancel())
    blacklistedPeers = blacklistedPeers.filterNot(_._1 == peer)
  }

  def isBlacklisted(peer: ActorRef): Boolean =
    blacklistedPeers.exists(_._1 == peer)
}

object BlacklistSupport {
  case class BlacklistPeer(peer: ActorRef, reason: String)
  case class UnblacklistPeer(peer: ActorRef)
}
