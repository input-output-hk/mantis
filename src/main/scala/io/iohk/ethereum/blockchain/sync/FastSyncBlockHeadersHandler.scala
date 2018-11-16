package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.blockchain.sync.FastSync._
import io.iohk.ethereum.blockchain.sync.FastSyncBlockHeadersValidator._
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

trait FastSyncBlockHeadersHandler extends FastSyncBlockHeadersValidator {

  def syncConfig: SyncConfig

  def handleBlockHeaders(
    peer: Peer,
    headers: Seq[BlockHeader],
    handlerState: FastSyncHandlerState,
    discardLastBlocks: (BigInt, Int) => AppStateStorage,
    blacklist: (BlackListId, FiniteDuration, String) => Unit
  ): (FastSyncHandlerState, FastSyncMsg) = {
    if (areHeadersFormingChain(headers)) {
      processHeaders(peer, headers, handlerState) match {
        case (newHandlerState, ParentDifficultyNotFound(header)) =>
          log.debug("Parent difficulty not found for block {}, not processing rest of headers", header.number)
          (newHandlerState, ProcessSyncing)

        case (newHandlerState, HeadersProcessingFinished) =>
          (newHandlerState, ProcessSyncing)

        case (newHandlerState, ImportedTargetBlock) =>
          (newHandlerState, UpdateTargetBlock(ImportedLastBlock))

        case (newHandlerState, ValidationFailed(header, peerToBlacklist)) =>
          blacklist(peer.id, syncConfig.blacklistDuration, "block header validation failed")
          handleBlockValidationError(header, peerToBlacklist, newHandlerState, discardLastBlocks)
      }
    } else {
      blacklist(peer.id, syncConfig.blacklistDuration, "error in block headers response")
      (handlerState, ProcessSyncing)
    }
  }

  @tailrec
  private def processHeaders(
    peer: Peer,
    headers: Seq[BlockHeader],
    handlerState: FastSyncHandlerState
  ): (FastSyncHandlerState, HeaderProcessingResult) = {
    if (headers.nonEmpty) {

      validateHeader(headers.head, peer, handlerState.syncState.nextBlockToFullyValidate) match {
        case Left(result) =>
          (handlerState, result)

        case Right((validHeader: BlockHeader, shouldUpdate: Boolean)) =>
          val withValidationStateUpdated =
            if (shouldUpdate) handlerState.updateValidationState(validHeader, syncConfig) else handlerState

          getParentDifficulty(validHeader) match {
            case Left(result) =>
              (withValidationStateUpdated, result)

            case Right(parentDifficulty) =>
              blockchain.save(validHeader)
              blockchain.save(validHeader.hash, parentDifficulty + validHeader.difficulty)

              val newHandlerState = withValidationStateUpdated.updateBestBlockNumber(validHeader)

              if (validHeader.number == newHandlerState.syncState.safeDownloadTarget){
                (newHandlerState, ImportedTargetBlock)
              } else {
                processHeaders(peer, headers.tail, newHandlerState)
              }
          }
      }
    } else {
      (handlerState, HeadersProcessingFinished)
    }
  }

  private def getParentDifficulty(header: BlockHeader): Either[ParentDifficultyNotFound, BigInt] = {
    blockchain.getTotalDifficultyByHash(header.parentHash).toRight(ParentDifficultyNotFound(header))
  }

  private def handleBlockValidationError(
    header: BlockHeader,
    peer: Peer,
    handlerState: FastSyncHandlerState,
    discardLastBlocks: (BigInt, Int) => AppStateStorage
  ): (FastSyncHandlerState, FastSyncMsg) = {
    val currentSyncState = handlerState.syncState
    val headerNumber = header.number
    if (headerNumber <= currentSyncState.safeDownloadTarget) {
      val n = syncConfig.fastSyncBlockValidationN
      discardLastBlocks(headerNumber, n)
      val newHandlerState = handlerState.withSyncState(currentSyncState.updateDiscardedBlocks(header, n))

      if (headerNumber >= newHandlerState.syncState.targetBlock.number) {
        (newHandlerState, UpdateTargetBlock(LastBlockValidationFailed))
      } else {
        (newHandlerState, ProcessSyncing)
      }
    } else {
      (handlerState, ProcessSyncing)
    }
  }
}
