package io.iohk.ethereum.nodebuilder.tooling

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.TimerScheduler

import scala.concurrent.duration.DurationInt

import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.BlockHeadersStorage
import io.iohk.ethereum.db.storage.BlockNumberMappingStorage
import io.iohk.ethereum.nodebuilder.tooling.PeriodicConsistencyCheck.ConsistencyCheck
import io.iohk.ethereum.utils.Logger

object PeriodicConsistencyCheck {
  def start(
      appStateStorage: AppStateStorage,
      blockNumberMappingStorage: BlockNumberMappingStorage,
      blockHeadersStorage: BlockHeadersStorage,
      shutdown: () => Unit
  ): Behavior[ConsistencyCheck] =
    Behaviors.withTimers { timers =>
      tick(timers)
      PeriodicConsistencyCheck(timers, appStateStorage, blockNumberMappingStorage, blockHeadersStorage, shutdown)
        .check()
    }

  sealed trait ConsistencyCheck extends Product with Serializable
  case object Tick extends ConsistencyCheck

  def tick(timers: TimerScheduler[ConsistencyCheck]): Unit =
    timers.startSingleTimer(Tick, 10.minutes)
}

case class PeriodicConsistencyCheck(
    timers: TimerScheduler[ConsistencyCheck],
    appStateStorage: AppStateStorage,
    blockNumberMappingStorage: BlockNumberMappingStorage,
    blockHeadersStorage: BlockHeadersStorage,
    shutdown: () => Unit
) extends Logger {
  import PeriodicConsistencyCheck._

  def check(): Behavior[ConsistencyCheck] = Behaviors.receiveMessage { case Tick =>
    log.debug("Running a storageConsistency check")
    StorageConsistencyChecker.checkStorageConsistency(
      appStateStorage.getBestBlockNumber(),
      blockNumberMappingStorage,
      blockHeadersStorage,
      shutdown
    )(log)
    tick(timers)
    Behaviors.same
  }
}
