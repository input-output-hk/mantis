package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.{Account, BlockHeader, Blockchain, Receipt}
import io.iohk.ethereum.network.Peer

import scala.annotation.tailrec
import FastSyncStateHandler._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSync.SyncState
import io.iohk.ethereum.blockchain.sync.FastSyncReceiptsValidator.ReceiptsValidationResult
import io.iohk.ethereum.blockchain.sync.SyncBlocksValidator.BlockBodyValidationResult
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, LeafNode, MptNode}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV63.NodeData
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.Logger
import org.bouncycastle.util.encoders.Hex
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._

import scala.util.{Failure, Success, Try}
import io.iohk.ethereum.crypto.kec256


class FastSyncStateHandler(
  val blockchain: Blockchain,
  val validators: Validators,
  val syncConfig: SyncConfig,
  val appStateStorage: AppStateStorage,
  val randomFun: Int => Int
) extends FastSyncReceiptsValidator with SyncBlocksValidator with Logger {

  def handleHeaders(peer: Peer, headers: Seq[BlockHeader], requestedHeaders: BigInt)(syncState: SyncState): (FastSyncCommand, SyncState) = {
    if (validHeaders(syncState ,headers, requestedHeaders))
      handleBlockHeaders(syncState, peer, headers)
    else
      (ContinueSyncing(Some(BlackListCommand(peer, "malformed blockheaders"))), syncState)
  }

  def updateTargetBlock(state: FinalBlockProcessingResult)(syncState: SyncState): (FastSyncCommand, SyncState) = {
    if (syncState.targetBlockUpdateFailures <= syncConfig.maximumTargetUpdateFailures) {
      (ContTargetBlockUpdate(state), syncState.copy(updatingTargetBlock = true))
    } else {
      (AbortFastSync, syncState)
    }
  }

  def handleNewTargetBlock(processState: FinalBlockProcessingResult,
                           targetBlockHeader: BlockHeader)(syncState: SyncState): (FastSyncCommand, SyncState) = {
    log.info(s"new target block with number ${targetBlockHeader.number} received")
    if (targetBlockHeader.number >= syncState.targetBlock.number) {
      val newState = updateTargetSyncState(syncState, processState, targetBlockHeader)
      (BackToSyncing, newState.copy(updatingTargetBlock = false))
    } else {
      (AskForNewTarget(processState), syncState.copy(targetBlockUpdateFailures = syncState.targetBlockUpdateFailures + 1))
    }
  }


  def handleBlockBodies(peer: Peer, requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody])(syncState: SyncState): (ContinueSyncing, SyncState) = {
    if (blockBodies.isEmpty) {
      val reason = s"got empty block bodies response for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}"
      (ContinueSyncing(Some(BlackListCommand(peer, reason))), syncState.enqueueBlockBodies(requestedHashes))
    } else {
      validateBlocks(requestedHashes, blockBodies) match {
        case BlockBodyValidationResult.Valid =>
          (ContinueSyncing(None), insertBlocks(syncState, requestedHashes, blockBodies))
        case BlockBodyValidationResult.Invalid =>
          val reason = s"responded with block bodies not matching block headers"
          (ContinueSyncing(Some(BlackListCommand(peer, reason))),syncState.enqueueBlockBodies(requestedHashes))
        case BlockBodyValidationResult.DbError =>
          (ContinueSyncing(None), redownloadBlockchain(syncState))
      }
    }
  }

  def handleReceipts(peer: Peer,
                     requestedHashes: Seq[ByteString],
                     receipts: Seq[Seq[Receipt]])(syncState: SyncState): (ContinueSyncing, SyncState) = {
    if (receipts.isEmpty) {
      val reason = s"got empty receipts for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}"
      (ContinueSyncing(Some(BlackListCommand(peer, reason))), syncState.enqueueReceipts(requestedHashes))
    } else {
      validateReceipts(requestedHashes, receipts) match {
        case ReceiptsValidationResult.Valid(blockHashesWithReceipts) =>
          blockHashesWithReceipts.foreach { case (hash, receiptsForBlock) =>
            blockchain.save(hash, receiptsForBlock)
          }
          val receivedHashes = blockHashesWithReceipts.unzip._1
          updateBestBlockIfNeeded(receivedHashes)
          val remainingReceipts = requestedHashes.drop(receipts.size)

          val newState =
            if (remainingReceipts.nonEmpty)
              syncState.enqueueReceipts(remainingReceipts)
            else
              syncState
          (ContinueSyncing(None), newState)

        case ReceiptsValidationResult.Invalid(error) =>
          val reason =
            s"got invalid receipts for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}" +
              s" due to: $error"
          (ContinueSyncing(Some(BlackListCommand(peer, reason))),syncState.enqueueReceipts(requestedHashes))

        case ReceiptsValidationResult.DbError =>
          (ContinueSyncing(None), redownloadBlockchain(syncState))
      }
    }
  }


  def handleNodeData(peer: Peer, requestedHashes: Seq[HashType], nodeData: NodeData)(syncState: SyncState): (ContinueSyncing, SyncState) = {
    if (nodeData.values.isEmpty) {
      log.debug(s"got empty mpt node response for known hashes switching to blockchain only: ${requestedHashes.map(h => Hex.toHexString(h.v.toArray[Byte]))}")
      val reason = "empty mpt node response for known hashes"
      (ContinueSyncing(Some(BlackListCommand(peer, reason))), syncState.addPendingNodes(requestedHashes))
    } else {
      val receivedHashes = nodeData.values.map(v => ByteString(kec256(v.toArray[Byte])))
      val remainingHashes = requestedHashes.filterNot(h => receivedHashes.contains(h.v))

      val syncState1 =
        if (remainingHashes.nonEmpty)
          syncState.addPendingNodes(remainingHashes)
        else
          syncState

      val hashesToRequest = (nodeData.values.indices zip receivedHashes) flatMap { case (idx, valueHash) =>
        requestedHashes.find(_.v == valueHash) map {
          case _: StateMptNodeHash =>
            handleMptNode(syncState1, nodeData.getMptNode(idx))

          case _: ContractStorageMptNodeHash =>
            handleContractMptNode(syncState1, nodeData.getMptNode(idx))

          case EvmCodeHash(hash) =>
            val evmCode = nodeData.values(idx)
            blockchain.save(hash, evmCode)
            Nil

          case StorageRootHash(_) =>
            val rootNode = nodeData.getMptNode(idx)
            handleContractMptNode(syncState1, rootNode)
        }
      }

      val finalState = syncState1
        .addPendingNodes(hashesToRequest.flatten)
        .copy(downloadedNodesCount = syncState.downloadedNodesCount + nodeData.values.size)


      (ContinueSyncing(None), finalState)
    }
  }

  def discardLastBlocks(startBlock: BigInt, blocksToDiscard: Int): Unit = {
    (startBlock to ((startBlock - blocksToDiscard) max 1) by -1).foreach { n =>
      blockchain.getBlockHeaderByNumber(n).foreach { headerToRemove =>
        blockchain.removeBlock(headerToRemove.hash, withState = false)
      }
    }
    appStateStorage.putBestBlockNumber((startBlock - blocksToDiscard - 1) max 0)
  }

  def requestNodes(from: Peer)(syncState: SyncState): (RequestNodes, SyncState) = {
    val (nonMptNodesToGet, remainingNonMptNodes) = syncState.pendingNonMptNodes.splitAt(syncConfig.nodesPerRequest)
    val (mptNodesToGet, remainingMptNodes) = syncState.pendingMptNodes.splitAt(syncConfig.nodesPerRequest - nonMptNodesToGet.size)
    val nodesToGet = nonMptNodesToGet ++ mptNodesToGet

    (RequestNodes(from, nodesToGet, mptNodesToGet, nonMptNodesToGet), syncState.copy(
      pendingNonMptNodes = remainingNonMptNodes,
      pendingMptNodes = remainingMptNodes
    ))
  }

  def requestHeaders(from: Peer)(syncState: SyncState): (RequestHeaders, SyncState) = {
    val numberOfHeaders: BigInt =
    if (syncConfig.blockHeadersPerRequest < (syncState.safeDownloadTarget - syncState.bestBlockHeaderNumber))
      syncConfig.blockHeadersPerRequest
    else
      syncState.safeDownloadTarget - syncState.bestBlockHeaderNumber

    (RequestHeaders(from, syncState.bestBlockHeaderNumber + 1, numberOfHeaders), syncState)
  }

  def requestBodies(from: Peer)(syncState: SyncState): (RequestBodies, SyncState) = {
    val (blockBodiesToGet, remainingBlockBodies) = syncState.blockBodiesQueue.splitAt(syncConfig.blockBodiesPerRequest)
    (RequestBodies(from, blockBodiesToGet), syncState.copy(blockBodiesQueue = remainingBlockBodies))
  }

  def requestReceipts(from: Peer)(syncState: SyncState): (RequestReceipts, SyncState) = {
    val (receiptsToGet, remainingReceipts) = syncState.receiptsQueue.splitAt(syncConfig.receiptsPerRequest)
    (RequestReceipts(from, receiptsToGet), syncState.copy(receiptsQueue = remainingReceipts))
  }

  private def handleMptNode(syncState: SyncState, mptNode: MptNode): Seq[HashType] = mptNode match {
    case n: LeafNode =>
      import io.iohk.ethereum.network.p2p.messages.PV63.AccountImplicits._
      //if this fails it means that we have leaf node which is part of MPT that do not stores account
      //we verify if node is paert of the tree by checking its hash before we call handleMptNode() in line 44
      val account = Try(n.value.toArray[Byte].toAccount) match {
        case Success(acc) => Some(acc)
        case Failure(e) =>
          log.debug(s"Leaf node without account, error while trying to decode account ${e.getMessage}")
          None
      }

      val evm = account.map(_.codeHash)
      val storage = account.map(_.storageRoot)

      blockchain.saveNode(ByteString(n.hash), n.toBytes, syncState.targetBlock.number, withSnapshotSave = false)

      val evmRequests = evm
        .filter(_ != Account.EmptyCodeHash)
        .map(c => Seq(EvmCodeHash(c))).getOrElse(Nil)

      val storageRequests = storage
        .filter(_ != Account.EmptyStorageRootHash)
        .map(s => Seq(StorageRootHash(s))).getOrElse(Nil)

      evmRequests ++ storageRequests

    case n: BranchNode =>
      val hashes = n.children.collect { case Some(Left(childHash)) => childHash }
      blockchain.saveNode(ByteString(n.hash), n.toBytes, syncState.targetBlock.number, withSnapshotSave = false)
      hashes.map(e => StateMptNodeHash(e))

    case n: ExtensionNode =>
      blockchain.saveNode(ByteString(n.hash), n.toBytes, syncState.targetBlock.number, withSnapshotSave = false)
      n.next.fold(
        mptHash => Seq(StateMptNodeHash(mptHash)),
        _ => Nil)
  }

  private def handleContractMptNode(syncState: SyncState, mptNode: MptNode): Seq[HashType] = {
    mptNode match {
      case n: LeafNode =>
        blockchain.saveNode(ByteString(n.hash), n.toBytes, syncState.targetBlock.number, withSnapshotSave = false)
        Nil

      case n: BranchNode =>
        val hashes = n.children.collect { case Some(Left(childHash)) => childHash }
        blockchain.saveNode(ByteString(n.hash), n.toBytes, syncState.targetBlock.number, withSnapshotSave = false)
        hashes.map(e => ContractStorageMptNodeHash(e))

      case n: ExtensionNode =>
        blockchain.saveNode(ByteString(n.hash), n.toBytes, syncState.targetBlock.number, withSnapshotSave = false)
        n.next.fold(
          mptHash => Seq(ContractStorageMptNodeHash(mptHash)),
          _ => Nil)
    }
  }

  /**
    * Restarts download from a few blocks behind the current best block header, as an unexpected DB error happened
    */
  private def redownloadBlockchain(syncState: SyncState): SyncState = {
    log.debug("missing block header for known hash")
    syncState.copy(
      blockBodiesQueue = Seq.empty,
      receiptsQueue = Seq.empty,
      //todo adjust the formula to minimize redownloaded block headers
      bestBlockHeaderNumber = (syncState.bestBlockHeaderNumber - 2 * syncConfig.blockHeadersPerRequest).max(0)
    )
  }

  private def insertBlocks(syncState: SyncState, requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): SyncState = {
    (requestedHashes zip blockBodies).foreach { case (hash, body) =>
      blockchain.save(hash, body)
    }

    val receivedHashes = requestedHashes.take(blockBodies.size)
    updateBestBlockIfNeeded(receivedHashes)
    val remainingBlockBodies = requestedHashes.drop(blockBodies.size)
    if (remainingBlockBodies.nonEmpty)
      syncState.enqueueBlockBodies(remainingBlockBodies)
    else
      syncState
  }

  private def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Unit = {
    val fullBlocks = receivedHashes.flatMap { hash =>
      for {
        header <- blockchain.getBlockHeaderByHash(hash)
        _ <- blockchain.getBlockBodyByHash(hash)
        _ <- blockchain.getReceiptsByHash(hash)
      } yield header
    }

    if (fullBlocks.nonEmpty) {
      val bestReceivedBlock = fullBlocks.maxBy(_.number)
      if (appStateStorage.getBestBlockNumber() < bestReceivedBlock.number) {
        appStateStorage.putBestBlockNumber(bestReceivedBlock.number)
      }
    }

  }

  private def validHeaders(syncState: SyncState, blockHeaders: Seq[BlockHeader], requestedNum: BigInt): Boolean =
    blockHeaders.nonEmpty &&
      blockHeaders.size <= requestedNum &&
      blockHeaders.head.number == (syncState.bestBlockHeaderNumber + 1) &&
      checkHeadersChain(blockHeaders)

  private def updateTargetSyncState(syncState: SyncState, state: FinalBlockProcessingResult, targetBlockHeader: BlockHeader): SyncState = state match {
    case ImportedLastBlock =>
      if (targetBlockHeader.number - syncState.targetBlock.number <= syncConfig.maxTargetDifference) {
        log.info(s"Current target block is fresh enough, starting state download")
        syncState.copy(pendingMptNodes = Seq(StateMptNodeHash(syncState.targetBlock.stateRoot)))
      } else {
        log.info(s"Changing target block to ${targetBlockHeader.number}, new safe target is ${targetBlockHeader.number + syncConfig.fastSyncBlockValidationX}")
        syncState.updateTargetBlock(targetBlockHeader, syncConfig.fastSyncBlockValidationX, updateFailures = false)
      }

    case LastBlockValidationFailed(_) =>
      log.info(s"Changing target block after failure, to ${targetBlockHeader.number}, new safe target is ${syncState.safeDownloadTarget}")
      syncState.updateTargetBlock(targetBlockHeader, syncConfig.fastSyncBlockValidationX, updateFailures = true)
  }

  private def handleBlockHeaders(syncState: SyncState, peer: Peer, headers: Seq[BlockHeader]): (FastSyncCommand, SyncState) = {
    processHeaders(syncState, peer, headers) match {
      case ParentDifficultyNotFound(header, finalState) =>
        log.debug("Parent difficulty not found for block {}, not processing rest of headers", header.number)
        (ContinueSyncing(None), finalState)
      case HeadersProcessingFinished(finalState) =>
        (ContinueSyncing(None), finalState)
      case ImportedTargetBlock(finalState)  =>
        (InitTargetBlockUpdate(ImportedLastBlock), finalState)
      case ValidationFailed(header, peerToBlackList, finalState) =>
        (handleBlockValidationError(finalState, header, peerToBlackList), finalState)
    }
  }

  @tailrec
  private def processHeaders(syncState: SyncState, peer: Peer, headers: Seq[BlockHeader]): HeaderProcessingResult = {
    if (headers.nonEmpty) {
      val header = headers.head
      processHeader(syncState, header, peer) match {
        case Left(result)        => result
        case Right(headerAndDif) =>
          val newState = updateSyncState(headerAndDif._1, headerAndDif._2)
          if (header.number == syncState.safeDownloadTarget){
            ImportedTargetBlock(newState)
          } else {
            processHeaders(newState ,peer, headers.tail)
          }
      }
    } else
      HeadersProcessingFinished(syncState)
  }

  private def processHeader(syncState: SyncState, header: BlockHeader, peer: Peer) : Either[HeaderProcessingResult , (ValidationSucces, BigInt)] = for {
    validatedHeader  <- validateHeader(syncState, header, peer)
    parentDifficulty <- blockchain.getTotalDifficultyByHash(header.parentHash).toRight(ParentDifficultyNotFound(header, validatedHeader.newState))
  } yield (validatedHeader, parentDifficulty)

  private def validateHeader(syncState: SyncState, header: BlockHeader, peer: Peer): Either[ValidationFailed, ValidationSucces] = {
    import syncConfig.{fastSyncBlockValidationK => K, fastSyncBlockValidationX => X, fastSyncBlockValidationN => N}
    val shouldValidate = header.number >= syncState.nextBlockToFullyValidate

    if (shouldValidate) {
      validators.blockHeaderValidator.validate(header, blockchain.getBlockHeaderByHash) match {
        case Right(_) =>
          Right(ValidationSucces(header, syncState.updateNextBlockToValidate(header, K, X, randomFun)))

        case Left(error) =>
          log.debug(s"Block header validation failed during fast sync at block ${header.number}: $error")
          Left(ValidationFailed(header, peer, syncState.updateDiscardedBlocks(header, N)))
      }
    } else {
      Right(ValidationSucces(header, syncState))
    }
  }

  private def updateSyncState(validatedHeader: ValidationSucces, parentTd: BigInt): SyncState = {
    blockchain.save(validatedHeader.header)
    blockchain.save(validatedHeader.header.hash, parentTd + validatedHeader.header.difficulty)

    validatedHeader.newState
      .enqueueBlockBodies(Seq(validatedHeader.header.hash))
      .enqueueReceipts(Seq(validatedHeader.header.hash))
      .copy(bestBlockHeaderNumber = validatedHeader.header.number)
  }

  private def handleBlockValidationError(syncState: SyncState, header: BlockHeader, peer: Peer): FastSyncCommand = {
    val reason = "peer send a header which did not pass validation"
    import syncConfig.{fastSyncBlockValidationN => N}
    if (header.number <= syncState.safeDownloadTarget) {
      discardLastBlocks(header.number, N)
      if (header.number >= syncState.targetBlock.number) {
        InitTargetBlockUpdate(LastBlockValidationFailed(BlackListCommand(peer, "failed validation of final headers")))
      } else {
        ContinueSyncing(Some(BlackListCommand(peer, reason)))
      }
    } else {
      ContinueSyncing(Some(BlackListCommand(peer, reason)))
    }
  }
}

// scalastyle:off
object FastSyncStateHandler {
  sealed trait HashType {
    def v: ByteString
  }

  case class StateMptNodeHash(v: ByteString) extends HashType
  case class ContractStorageMptNodeHash(v: ByteString) extends HashType
  case class EvmCodeHash(v: ByteString) extends HashType
  case class StorageRootHash(v: ByteString) extends HashType

  sealed abstract class HeaderProcessingResult{
    def newState: SyncState
  }
  case class  HeadersProcessingFinished(newState: SyncState)                         extends HeaderProcessingResult
  case class  ParentDifficultyNotFound(header:BlockHeader, newState: SyncState)      extends HeaderProcessingResult
  case class  ValidationFailed(header:BlockHeader, peer: Peer, newState: SyncState)  extends HeaderProcessingResult
  case class  ImportedTargetBlock(newState: SyncState)                               extends HeaderProcessingResult

  case class  ValidationSucces(header:BlockHeader, newState: SyncState)


  sealed abstract class FinalBlockProcessingResult
  case object ImportedLastBlock         extends FinalBlockProcessingResult
  case class LastBlockValidationFailed(blackListCommand: BlackListCommand) extends FinalBlockProcessingResult


  case class BlackListCommand(peer: Peer, reason: String)
  sealed abstract class FastSyncCommand
  case class ContinueSyncing(peerToBlacklist: Option[BlackListCommand])   extends FastSyncCommand
  case class InitTargetBlockUpdate(reason: FinalBlockProcessingResult)    extends FastSyncCommand
  case object AbortFastSync                                               extends FastSyncCommand
  case class ContTargetBlockUpdate(reason: FinalBlockProcessingResult)    extends FastSyncCommand
  case object BackToSyncing                                               extends FastSyncCommand
  case class AskForNewTarget(reason: FinalBlockProcessingResult)          extends FastSyncCommand
  case class RequestReceipts(from: Peer, receiptsToGet: Seq[ByteString])  extends FastSyncCommand
  case class RequestBodies(from: Peer, bodiesToGet: Seq[ByteString])      extends FastSyncCommand
  case class RequestNodes(from: Peer,
                          nodesToGet: Seq[HashType],
                          mptNodesToGet: Seq[HashType],
                          nonMptNodesToGet: Seq[HashType])                 extends FastSyncCommand
  case class RequestHeaders(from: Peer, starting: BigInt, number: BigInt)  extends FastSyncCommand


}
