package io.iohk.ethereum.sync.util

import java.net.{InetSocketAddress, ServerSocket}
import java.util.concurrent.TimeoutException

import akka.util.ByteString
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.domain.{Account, Address, Block, Receipt}
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess, InMemoryWorldStateProxy}
import io.iohk.ethereum.nodebuilder.ShutdownHookBuilder
import monix.eval.Task
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.FiniteDuration

object SyncUtils {

  object ShutdownHookBuilder extends ShutdownHookBuilder with Logger
  class ValidatorsExecutorAlwaysSucceed extends MockValidatorsAlwaysSucceed {
    override def validateBlockAfterExecution(block: Block, stateRootHash: ByteString, receipts: Seq[Receipt], gasUsed: BigInt)
    : Either[BlockExecutionError, BlockExecutionSuccess] = Right(BlockExecutionSuccess)
  }

  object ValidatorsExecutorAlwaysSucceed extends ValidatorsExecutorAlwaysSucceed

  def retryUntilWithDelay[A](source: Task[A], delay: FiniteDuration, maxRetries: Int)(
    predicate: A => Boolean
  ): Task[A] = {
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
  }

  def randomAddress(): InetSocketAddress = {
    val s = new ServerSocket(0)
    try {
      new InetSocketAddress("localhost", s.getLocalPort)
    } finally {
      s.close()
    }
  }

  final case class BlockchainState(bestBlock: Block, currentWorldState: InMemoryWorldStateProxy, currentTd: BigInt)

  def identityUpdate: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy = (_, world) => world

  def updateWorldWithNAccounts(n: Int, world: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    val resultWorld = (0 until n).foldLeft(world) { (world, num) =>
      val randomBalance = num
      val randomAddress = Address(num)
      val codeBytes = BigInt(num).toByteArray
      val storage = world.getStorage(randomAddress)
      val changedStorage = (num until num + 20).foldLeft(storage)((storage, value) => storage.store(value, value))
      world
        .saveAccount(randomAddress, Account.empty().copy(balance = randomBalance))
        .saveCode(randomAddress, ByteString(codeBytes))
        .saveStorage(randomAddress, changedStorage)
    }
    InMemoryWorldStateProxy.persistState(resultWorld)
  }

  def updateStateAtBlock(blockWithUpdate: BigInt): (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy = {
    (blockNr: BigInt, world: InMemoryWorldStateProxy) =>
      if (blockNr == blockWithUpdate) {
        updateWorldWithNAccounts(1000, world)
      } else {
        identityUpdate(blockNr, world)
      }
  }
}
