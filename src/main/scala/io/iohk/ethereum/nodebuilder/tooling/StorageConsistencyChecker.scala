package io.iohk.ethereum.nodebuilder.tooling

import com.typesafe.scalalogging.Logger

import io.iohk.ethereum.db.storage.BlockHeadersStorage
import io.iohk.ethereum.db.storage.BlockNumberMappingStorage

object StorageConsistencyChecker {
  type ShutdownOp = () => Unit

  val DefaultStep = 1000

  def checkStorageConsistency(
      bestBlockNumber: BigInt,
      blockNumberMappingStorage: BlockNumberMappingStorage,
      blockHeadersStorage: BlockHeadersStorage,
      shutdown: ShutdownOp,
      step: Int = DefaultStep
  )(implicit log: Logger): Unit =
    Range(0, bestBlockNumber.intValue, step).foreach { idx =>
      (for {
        hash <- blockNumberMappingStorage.get(idx)
        _ <- blockHeadersStorage.get(hash)
      } yield ()).fold {
        log.error("Database seems to be in inconsistent state, shutting down")
        shutdown()
      }(_ => ())
    }
}
