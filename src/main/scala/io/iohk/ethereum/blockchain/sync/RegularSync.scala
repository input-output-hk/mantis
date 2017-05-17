package io.iohk.ethereum.blockchain.sync

import akka.actor._
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlacklistPeer
import io.iohk.ethereum.blockchain.sync.SyncRequestHandler.Done
import io.iohk.ethereum.blockchain.sync.SyncController._
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.ledger.BlockExecutionError
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerActor._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.utils.Config
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global

trait RegularSync {
  selfSyncController: SyncController =>

  private var headersQueue: Seq[BlockHeader] = Nil
  private var broadcasting = false
  private var waitingForActor: Option[ActorRef] = None

  import Config.FastSync._

  def startRegularSync(): Unit = {
    log.info("Starting regular sync")
    appStateStorage.fastSyncDone()
    context become (handlePeerUpdates orElse regularSync())
    askForHeaders()
  }

  def regularSync(): Receive = {
    case ResumeRegularSync =>
      askForHeaders()

    case BlockHeadersToResolve(peer, headers) =>
      waitingForActor = None
      handleBlockBranchResolution(peer, headers)

    case BlockHeadersReceived(peer, headers) =>
      waitingForActor = None
      handleDownload(peer, headers)

    case BlockBodiesReceived(peer, _, blockBodies) =>
      waitingForActor = None
      handleBlockBodies(peer, blockBodies)

    case block: BroadcastBlocks if broadcasting =>
      //FIXME: Decide block propagation algorithm (for now we send block to every peer) [EC-87]
      peersToDownloadFrom.keys.foreach(_ ! block)

    case MinedBlock(block) if headersQueue.isEmpty && waitingForActor.isEmpty =>
      //we are at the top of chain we can insert new block
      blockchain.getBlockHeaderByHash(block.header.parentHash)
        .flatMap(b => blockchain.getTotalDifficultyByHash(b.hash)) match {
        case Some(parentTd) =>
          blockchain.save(block)
          appStateStorage.putBestBlockNumber(block.header.number)
          val newTd = parentTd + block.header.difficulty
          blockchain.save(block.header.hash, newTd)
          log.info(s"added new block $block")
        case _ =>
          log.error("fail to add mined block")
      }

    case PrintStatus =>
      log.info(s"Peers: ${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).")

    case Done =>
      if (waitingForActor == Option(sender())) {
        //actor is done and we did not get response
        scheduleResume()
      }
  }

  private def askForHeaders() = {
    bestPeer match {
      case Some(peer) =>
        val blockNumber = appStateStorage.getBestBlockNumber()
        val request = GetBlockHeaders(Left(blockNumber + 1), blockHeadersPerRequest, skip = 0, reverse = false)
        waitingForActor = Some(context.actorOf(SyncBlockHeadersRequestHandler.props(peer, request, resolveBranches = false)))
      case None =>
        log.warning("no peers to download from")
        scheduleResume()
    }
  }

  private def handleBlockBranchResolution(peer: ActorRef, message: Seq[BlockHeader]) =
    //todo limit max branch depth?
    if (message.nonEmpty && message.last.hash == headersQueue.head.parentHash) {
      headersQueue = message ++ headersQueue
      processBlockHeaders(peer, headersQueue)
    } else {
      //we did not get previous blocks, there is no way to resolve, blacklist peer and continue download
      resumeWithDifferentPeer(peer)
    }

  private def handleDownload(peer: ActorRef, message: Seq[BlockHeader]) = if (message.nonEmpty) {
    headersQueue = message
    processBlockHeaders(peer, message)
  } else {
    //no new headers to process, schedule to ask again in future, we are at the top of chain
    broadcasting = true
    scheduleResume()
  }

  private def processBlockHeaders(peer: ActorRef, headers: Seq[BlockHeader]) = {
    val parentByNumber = blockchain.getBlockHeaderByNumber(headers.head.number - 1)

    parentByNumber match {
      case Some(parent) if checkHeaders(headers) =>
        //we have same chain
        if (parent.hash == headers.head.parentHash) {
          val hashes = headersQueue.take(blockBodiesPerRequest).map(_.hash)
          //TODO compare weights of branches
          waitingForActor = Some(context.actorOf(SyncBlockBodiesRequestHandler.props(peer, hashes)))
        } else {
          val request = GetBlockHeaders(Right(headersQueue.head.parentHash), blockResolveDepth, skip = 0, reverse = true)
          waitingForActor = Some(context.actorOf(SyncBlockHeadersRequestHandler.props(peer, request, resolveBranches = true)))
        }
      case _ =>
        log.warning("got header that does not have parent")
        resumeWithDifferentPeer(peer)
    }
  }

  private def handleBlockBodies(peer: ActorRef, m: Seq[BlockBody]) = {
    if (m.nonEmpty && headersQueue.nonEmpty) {
      val blocks = headersQueue.zip(m).map{ case (header, body) => Block(header, body) }

      blockchain.getBlockHeaderByHash(blocks.head.header.parentHash)
        .flatMap(b => blockchain.getTotalDifficultyByHash(b.hash)) match {
        case Some(blockParentTd) =>
          val (newBlocks, errorOpt) = processBlocks(blocks, blockParentTd)

          if(newBlocks.nonEmpty){
            context.self ! BroadcastBlocks(newBlocks)
            log.info(s"got new blocks up till block: ${newBlocks.last.block.header.number} " +
              s"with hash ${Hex.toHexString(newBlocks.last.block.header.hash.toArray[Byte])}")
          }

          errorOpt match {
            case Some(error) =>
              val numberBlockFailed = blocks.head.header.number + newBlocks.length
              resumeWithDifferentPeer(peer, reason = s"a block execution error: ${error.toString}, in block $numberBlockFailed")
            case None =>
              headersQueue = headersQueue.drop(blocks.length)
              if (headersQueue.nonEmpty) {
                val hashes = headersQueue.take(blockBodiesPerRequest).map(_.hash)
                waitingForActor = Some(context.actorOf(SyncBlockBodiesRequestHandler.props(peer, hashes)))
              } else {
                context.self ! ResumeRegularSync
              }
          }
        case None =>
          //TODO: Investigate if we can recover from this error (EC-165)
          val parentHash = Hex.toHexString(blocks.head.header.parentHash.toArray)
          throw new IllegalStateException(s"No total difficulty for the latest block with number ${blocks.head.header.number - 1} (and hash $parentHash)")
      }

    } else {
      //we got empty response for bodies from peer but we got block headers earlier
      resumeWithDifferentPeer(peer)
    }
  }

  /**
    * Inserts and executes all the blocks, up to the point to which one of them fails (or we run out of blocks).
    * If the execution of any block were to fail, newBlocks only contains the NewBlock msgs for all the blocks executed before it,
    * and only the blocks successfully executed are inserted into the blockchain.
    *
    * @param blocks to execute
    * @param blockParentTd, td of the parent of the blocks.head block
    * @param newBlocks which, after adding the corresponding NewBlock msg for blocks, will be broadcasted
    * @return list of NewBlocks to broadcast (one per block successfully executed) and an error if one happened during execution
    */
  @tailrec
  private def processBlocks(blocks: Seq[Block], blockParentTd: BigInt,
                           newBlocks: Seq[NewBlock] = Nil): (Seq[NewBlock], Option[BlockExecutionError]) = blocks match {
    case Nil =>
      newBlocks -> None

    case Seq(block, otherBlocks@_*) =>
      val blockHashToDelete = blockchain.getBlockHeaderByNumber(block.header.number).map(_.hash).filter(_ != block.header.hash)
      val blockExecResult = ledger.executeBlock(block, blockchainStorages, validators)
      blockExecResult match {
        case Right(_) =>
          blockchain.save(block)
          appStateStorage.putBestBlockNumber(block.header.number)
          val newTd = blockParentTd + block.header.difficulty
          blockchain.save(block.header.hash, newTd)
          blockHashToDelete.foreach(blockchain.removeBlock)
          processBlocks(otherBlocks, newTd, newBlocks :+ NewBlock(block, newTd))

        case Left(error) =>
          newBlocks -> Some(error)
      }
  }

  private def scheduleResume() = {
    headersQueue = Nil
    scheduler.scheduleOnce(checkForNewBlockInterval, context.self, ResumeRegularSync)
  }

  private def resumeWithDifferentPeer(currentPeer: ActorRef, reason: String = "error in response") = {
    self ! BlacklistPeer(currentPeer, "because of " + reason)
    headersQueue = Nil
    context.self ! ResumeRegularSync
  }

  private def checkHeaders(headers: Seq[BlockHeader]): Boolean =
    headers.zip(headers.tail).forall { case (parent, child) => parent.hash == child.parentHash && parent.number + 1 == child.number }

  private def bestPeer: Option[ActorRef] = {
    val peersToUse = peersToDownloadFrom
      .collect {
        case (ref, Handshaked(_, true, totalDifficulty)) => (ref, totalDifficulty)
      }

    if (peersToUse.nonEmpty) Some(peersToUse.maxBy { case (_, td) => td }._1)
    else None
  }

  private case object ResumeRegularSync
  private case class ResolveBranch(peer: ActorRef)
}
