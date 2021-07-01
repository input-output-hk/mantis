package io.iohk.ethereum.sync.util

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.TimeoutException

import monix.eval.Task

import scala.concurrent.duration.FiniteDuration

import io.iohk.ethereum.network.PeerManagerActor.FastSyncHostConfiguration

object SyncCommonItSpecUtils {
  def retryUntilWithDelay[A](source: Task[A], delay: FiniteDuration, maxRetries: Int)(
      predicate: A => Boolean
  ): Task[A] =
    source.delayExecution(delay).flatMap { result =>
      if (predicate(result)) {
        Task.now(result)
      } else {
        if (maxRetries > 0) {
          retryUntilWithDelay(source, delay, maxRetries - 1)(predicate)
        } else {
          Task.raiseError(new TimeoutException("Task time out after all retries"))
        }
      }
    }

  case class HostConfig(
      maxBlocksHeadersPerMessage: Int,
      maxBlocksBodiesPerMessage: Int,
      maxReceiptsPerMessage: Int,
      maxMptComponentsPerMessage: Int
  ) extends FastSyncHostConfiguration

  object HostConfig {
    def apply(): HostConfig = {
      val random: ThreadLocalRandom = ThreadLocalRandom.current()
      new HostConfig(
        maxBlocksHeadersPerMessage = random.nextInt(100, 201),
        maxBlocksBodiesPerMessage = random.nextInt(30, 51),
        maxReceiptsPerMessage = random.nextInt(30, 51),
        maxMptComponentsPerMessage = random.nextInt(100, 201)
      )
    }
  }

  final case class FakePeerCustomConfig(hostConfig: HostConfig)

  object FakePeerCustomConfig {
    val defaultConfig: FakePeerCustomConfig = FakePeerCustomConfig(HostConfig(200, 200, 200, 200))
  }
}
