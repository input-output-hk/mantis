package io.iohk.ethereum.domain

import java.util.concurrent.atomic.AtomicReference

class BlockchainMetadata(bestBlockNumber: BigInt, latestCheckpointNumber: BigInt) {
  lazy val bestKnownBlockAndLatestCheckpoint: AtomicReference[BestBlockLatestCheckpointNumbers] =
    new AtomicReference(BestBlockLatestCheckpointNumbers(bestBlockNumber, latestCheckpointNumber))
}

case class BestBlockLatestCheckpointNumbers(bestBlockNumber: BigInt, latestCheckpointNumber: BigInt)
