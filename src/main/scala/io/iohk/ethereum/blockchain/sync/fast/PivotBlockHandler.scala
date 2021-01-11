package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{ActorContext, ActorRef}
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.PeerListSupport.PeersMap
import io.iohk.ethereum.blockchain.sync.fast.FastSync._
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.utils.Config.SyncConfig
import scala.concurrent.ExecutionContextExecutor

class PivotBlockHandler(
    var pivotBlock: BlockHeader,
    peerEventBus: ActorRef,
    etcPeerManager: ActorRef,
    storage: SyncingHandlerStorage,
    syncConfig: SyncConfig
)(implicit context: ActorContext) {

  private val log = context.system.log
  private val scheduler = context.system.scheduler
  private implicit val ec: ExecutionContextExecutor = context.dispatcher

  var safeDownloadTarget: BigInt = 0
  var updatingPivotBlock: Boolean = false
  var pivotBlockUpdateFailures: Int = 0
  var stateSyncRestartRequested: Boolean = false
  var stateSyncFinished: Boolean = false

  updatePivotBlock(pivotBlock, SyncRestart)

  def askForPivotBlockUpdate(): Unit = {
    updatingPivotBlock = true
    log.info("Asking for new pivot block")
    val pivotBlockSelector: ActorRef = context.actorOf(
      PivotBlockSelector.props(etcPeerManager, peerEventBus, syncConfig, scheduler, context.self),
      "pivot-block-selector"
    )
    pivotBlockSelector ! PivotBlockSelector.SelectPivotBlock
  }

  def newPivotIsGoodEnough(newPivot: BlockHeader, updateReason: PivotBlockUpdateReason): Boolean = {
    newPivot.number >= pivotBlock.number && !stalePivotAfterRestart(newPivot, updateReason)
  }

  private def stalePivotAfterRestart(newPivot: BlockHeader, updateReason: PivotBlockUpdateReason): Boolean = {
    newPivot.number == pivotBlock.number && updateReason.isSyncRestart
  }

  def updatePivotSyncState(updateReason: PivotBlockUpdateReason, pivotBlockHeader: BlockHeader): Unit = {
    if (updateReason == ImportedLastBlock && pivotIsFreshEnough(pivotBlockHeader)) {
      log.info(s"Current pivot block is fresh enough, starting state download")
      startStateDownload(pivotBlockHeader)
    } else {
      updatePivotBlock(pivotBlockHeader, updateReason)
    }
  }

  private def updatePivotBlock(pivotBlockHeader: BlockHeader, reason: PivotBlockUpdateReason): Unit = {
    log.info(
      s"Changing pivot block to ${pivotBlockHeader.number} because of $reason, new safe target is $safeDownloadTarget"
    )
    pivotBlock = pivotBlockHeader
    safeDownloadTarget = pivotBlockHeader.number + syncConfig.fastSyncBlockValidationX
    if (reason == LastBlockValidationFailed) pivotBlockUpdateFailures += 1
    updatingPivotBlock = false
  }

  private def pivotIsFreshEnough(pivotBlockHeader: BlockHeader) = {
    pivotBlockHeader.number - pivotBlock.number <= syncConfig.maxTargetDifference
  }

  private def startStateDownload(pivotBlockHeader: BlockHeader): Unit = {
    // Empty root has means that there were no transactions in blockchain, and Mpt trie is empty
    // Asking for this root would result only with empty transactions
    if (pivotBlock.stateRoot == ByteString(MerklePatriciaTrie.EmptyRootHash)) {
      finishStateSync()
      updatingPivotBlock = false
    } else {
      updatingPivotBlock = false
      stateSyncRestartRequested = false
      storage.startSyncingTo(pivotBlockHeader)(context.self)
    }
  }

  def finishStateSync(): Unit = {
    stateSyncFinished = true
  }

  def reScheduleAskForNewPivot(updateReason: PivotBlockUpdateReason): Unit = {
    pivotBlockUpdateFailures += 1
    scheduler
      .scheduleOnce(syncConfig.pivotBlockReScheduleInterval, context.self, UpdatePivotBlock(updateReason))
  }

  def maxAttemptsReached: Boolean = pivotBlockUpdateFailures > syncConfig.maximumTargetUpdateFailures

  def notInTheMiddleOfUpdate: Boolean = !(updatingPivotBlock || stateSyncRestartRequested)

  def pivotBlockIsStale(peersToDownloadFrom: PeersMap): Boolean = {
    val currentPeers = peersToDownloadFrom.toList

    if (currentPeers.isEmpty) {
      false
    } else {
      val peerWithBestBlockInNetwork = currentPeers.maxBy(peerWithNum => peerWithNum._2.maxBlockNumber)

      val bestPossibleTargetDifferenceInNetwork =
        (peerWithBestBlockInNetwork._2.maxBlockNumber - syncConfig.pivotBlockOffset) - pivotBlock.number

      val peersWithTooFreshPossiblePivotBlock =
        getPeersWithFreshEnoughPivot(NonEmptyList.fromListUnsafe(currentPeers), syncConfig)

      if (peersWithTooFreshPossiblePivotBlock.isEmpty) {
        log.info(
          s"There are no peers with too fresh possible pivot block. " +
            s"Current pivot block is $bestPossibleTargetDifferenceInNetwork blocks behind best possible target"
        )
        false
      } else {
        val pivotBlockIsStale = peersWithTooFreshPossiblePivotBlock.size >= syncConfig.minPeersToChoosePivotBlock

        log.info(
          s"There are ${peersWithTooFreshPossiblePivotBlock.size} peers with possible new pivot block, " +
            s"best known pivot in current peer list has number ${peerWithBestBlockInNetwork._2.maxBlockNumber}"
        )

        pivotBlockIsStale
      }
    }
  }

  private def getPeersWithFreshEnoughPivot(
      peers: NonEmptyList[(Peer, PeerInfo)],
      syncConfig: SyncConfig
  ): List[(Peer, BigInt)] = {
    peers.collect {
      case (peer, info) if hasBestBlockFreshEnoughToUpdatePivotBlock(info, syncConfig) => (peer, info.maxBlockNumber)
    }
  }

  def hasBestBlockFreshEnoughToUpdatePivotBlock(info: PeerInfo, syncConfig: SyncConfig): Boolean = {
    (info.maxBlockNumber - syncConfig.pivotBlockOffset) - pivotBlock.number >= syncConfig.maxPivotBlockAge
  }
}
