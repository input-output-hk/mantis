package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.{Cancellable, ActorRef, Actor}

trait BlacklistSupport {
  selfActor: Actor =>

  import BlacklistSupport._
  import context.system

  var blacklistedPeers: Seq[(ActorRef, Cancellable)] = Nil

  def blacklist(peer: ActorRef, duration: FiniteDuration): Unit = {
    undoBlacklist(peer)
    val unblacklistCancellable = system.scheduler.scheduleOnce(duration, self, UnblacklistPeer(peer))
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
  case class BlacklistPeer(peer: ActorRef)
  case class UnblacklistPeer(peer: ActorRef)
}
