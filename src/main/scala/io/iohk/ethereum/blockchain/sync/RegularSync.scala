package io.iohk.ethereum.blockchain.sync

import akka.actor._
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlacklistPeer
import io.iohk.ethereum.blockchain.sync.SyncRequestHandler.Done
import io.iohk.ethereum.blockchain.sync.SyncController.{BlockBodiesReceived, BlockHeadersReceived, BlockHeadersToResolve, PrintStatus}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerActor.Status.{Chain, Handshaked}
import io.iohk.ethereum.network.PeerActor._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.utils.Config
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global

trait RegularSync {
  selfSyncController: SyncController =>

  private var headersQueue: Seq[BlockHeader] = Seq.empty
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
          waitingForActor = Some(context.actorOf(SyncBlockBodiesRequestHandler.props(peer, hashes, appStateStorage)))
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
    if (m.nonEmpty) {
      val result = headersQueue.zip(m).map { case (h, b) => blockValidator(h, b) }

      if (!result.exists(_.isLeft) && result.nonEmpty) {
        val blocks = result.collect { case Right(b) => b }

        blockchain.getBlockHeaderByHash(blocks.head.header.parentHash)
          .flatMap(b => blockchain.getTotalDifficultyByHash(b.hash)) match {
          case Some(td) =>
            var currentTd = td
            val newBlocks = blocks.map { b =>
              val blockHashToDelete = blockchain.getBlockHeaderByNumber(b.header.number).map(_.hash).filter(_ != b.header.hash)
              blockchain.save(b)
              appStateStorage.putBestBlockNumber(b.header.number)
              currentTd += b.header.difficulty
              blockchain.save(b.header.hash, currentTd)
              blockHashToDelete.foreach(blockchain.removeBlock)

              NewBlock(b, currentTd)
            }

            context.self ! BroadcastBlocks(newBlocks)
            log.info(s"got new blocks up till block: ${newBlocks.last.block.header.number} " +
              s"with hash ${Hex.toHexString(newBlocks.last.block.header.hash.toArray[Byte])}")
          case None =>
            log.error("no total difficulty for latest block")
        }

        headersQueue = headersQueue.drop(result.length)
        if (headersQueue.nonEmpty) {
          val hashes = headersQueue.take(blockBodiesPerRequest).map(_.hash)
          waitingForActor = Some(context.actorOf(SyncBlockBodiesRequestHandler.props(peer, hashes, appStateStorage)))
        } else {
          context.self ! ResumeRegularSync
        }
      } else {
        //blacklist for errors in blocks
        resumeWithDifferentPeer(peer)
      }
    } else {
      //we got empty response for bodies from peer but we got block headers earlier
      resumeWithDifferentPeer(peer)
    }
  }

  private def scheduleResume() = {
    headersQueue = Seq.empty
    scheduler.scheduleOnce(checkForNewBlockInterval, context.self, ResumeRegularSync)
  }

  private def resumeWithDifferentPeer(currentPeer: ActorRef) = {
    log.info(s"Blacklisting peer (${currentPeer.path.name}), because of error in response")
    self ! BlacklistPeer(currentPeer)
    headersQueue = Seq.empty
    context.self ! ResumeRegularSync
  }

  private def checkHeaders(headers: Seq[BlockHeader]): Boolean =
    headers.zip(headers.tail).forall { case (parent, child) => parent.hash == child.parentHash && parent.number + 1 == child.number }

  private def bestPeer: Option[ActorRef] = {
    val peersToUse = peersToDownloadFrom
      .collect { case (ref, Handshaked(_, Chain.ETC, totalDifficulty)) => (ref, totalDifficulty) }

    if (peersToUse.nonEmpty) Some(peersToUse.maxBy { case (_, td) => td }._1)
    else None
  }

  private case object ResumeRegularSync
  private case class ResolveBranch(peer: ActorRef)
}
