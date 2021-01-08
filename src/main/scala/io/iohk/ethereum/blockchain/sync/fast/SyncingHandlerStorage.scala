package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.fast.FastSync.ParentChainWeightNotFound
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor.{RestartRequested, StartSyncingTo}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{BlockBody, BlockHeader, Blockchain, ChainWeight, Receipt}

class SyncingHandlerStorage(
    syncStateStorage: ActorRef,
    syncStateScheduler: ActorRef,
    appStateStorage: AppStateStorage,
    blockchain: Blockchain) {

  def persistSyncState(
      syncState: SyncState,
      requestedBlockBodies: Seq[ByteString],
      requestedReceipts: Seq[ByteString]): Unit = {
    syncStateStorage ! syncState.copy(
      blockBodiesQueue = requestedBlockBodies.distinct ++ syncState.blockBodiesQueue,
      receiptsQueue = requestedReceipts.distinct ++ syncState.receiptsQueue
    )
  }

  def discardLastBlocks(startBlock: BigInt, blocksToDiscard: Int): Unit = {
    (startBlock to ((startBlock - blocksToDiscard) max 1) by -1).foreach { n =>
      blockchain.getBlockHeaderByNumber(n).foreach { headerToRemove =>
        blockchain.removeBlock(headerToRemove.hash, withState = false)
      }
    }
    // TODO (maybe ETCM-77): Manage last checkpoint number too
    appStateStorage.putBestBlockNumber((startBlock - blocksToDiscard - 1) max 0).commit()
  }

  def persistFastSyncDone(): Unit = {
    appStateStorage.fastSyncDone().commit()
  }

  def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Option[BigInt] = {
    val fullBlocks = getFullBlocks(receivedHashes)

    if (fullBlocks.nonEmpty) {
      val bestReceivedBlock = fullBlocks.maxBy(_.number)
      val lastStoredBestBlockNumber = appStateStorage.getBestBlockNumber()
      if (lastStoredBestBlockNumber < bestReceivedBlock.number) {
        blockchain.saveBestKnownBlocks(bestReceivedBlock.number)
        appStateStorage.putBestBlockNumber(bestReceivedBlock.number).commit()
      }
      Some(bestReceivedBlock.number.max(lastStoredBestBlockNumber))
    }
    else None
  }

  private def getFullBlocks(receivedHashes: Seq[ByteString]): Seq[BlockHeader] = {
    receivedHashes.flatMap { hash =>
      for {
        header <- blockchain.getBlockHeaderByHash(hash)
        _ <- blockchain.getBlockBodyByHash(hash)
        _ <- blockchain.getReceiptsByHash(hash)
      } yield header
    }
  }

  def getBestBlockNumber(): BigInt = appStateStorage.getBestBlockNumber()

  def updateSyncState(header: BlockHeader, parentWeight: ChainWeight): Unit = {
    blockchain
      .storeBlockHeader(header)
      .and(blockchain.storeChainWeight(header.hash, parentWeight.increase(header)))
      .commit()
  }

  def getParentChainWeight(header: BlockHeader): Either[ParentChainWeightNotFound, ChainWeight] = {
    blockchain.getChainWeightByHash(header.parentHash).toRight(ParentChainWeightNotFound(header))
  }

  def storeReceipts(blockHashesWithReceipts: Seq[(ByteString, Seq[Receipt])]): Unit = {
    blockHashesWithReceipts
      .map { case (hash, receiptsForBlock) =>
        blockchain.storeReceipts(hash, receiptsForBlock)
      }
      .reduce(_.and(_))
      .commit()
  }

  def storeBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): Unit = {
    (requestedHashes zip blockBodies)
      .map { case (hash, body) =>
        blockchain.storeBlockBody(hash, body)
      }
      .reduce(_.and(_))
      .commit()
  }

  def startSyncingTo(pivotBlockHeader: BlockHeader): Unit = {
    syncStateScheduler ! StartSyncingTo(pivotBlockHeader.stateRoot, pivotBlockHeader.number)
  }

  def requestSyncRestart(): Unit = {
    syncStateScheduler ! RestartRequested
  }
}
