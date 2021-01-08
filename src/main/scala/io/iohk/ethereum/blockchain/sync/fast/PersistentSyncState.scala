package io.iohk.ethereum.blockchain.sync.fast

import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader

case class PersistentSyncState(
    pivotBlock: BlockHeader,
    lastFullBlockNumber: BigInt = 0,
    safeDownloadTarget: BigInt = 0,
    blockBodiesQueue: Seq[ByteString] = Nil,
    receiptsQueue: Seq[ByteString] = Nil,
    downloadedNodesCount: Long = 0,
    totalNodesCount: Long = 0,
    bestBlockHeaderNumber: BigInt = 0,
    nextBlockToFullyValidate: BigInt = 1,
    pivotBlockUpdateFailures: Int = 0,
    updatingPivotBlock: Boolean = false,
    stateSyncFinished: Boolean = false
)
