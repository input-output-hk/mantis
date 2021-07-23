package io.iohk.ethereum.domain

import io.iohk.ethereum.domain.appstate.BestBlockInfo

import java.util.concurrent.atomic.AtomicReference

class BlockchainMetadata(bestBlockData: BestBlockInfo, latestCheckpointNumber: BigInt) {
  lazy val bestKnownBlockAndLatestCheckpoint: AtomicReference[BestBlockLatestCheckpointNumbers] =
    new AtomicReference(BestBlockLatestCheckpointNumbers(bestBlockData, latestCheckpointNumber))
}

case class BestBlockLatestCheckpointNumbers(bestBlockInfo: BestBlockInfo, latestCheckpointNumber: BigInt)
