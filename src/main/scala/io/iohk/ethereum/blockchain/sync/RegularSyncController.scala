package io.iohk.ethereum.blockchain.sync

import akka.actor._
import io.iohk.ethereum.blockchain.sync.SyncController.StartRegularSync
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerActor.Status.{Chain, Handshaked}
import io.iohk.ethereum.network.PeerActor._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.utils.Config
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

trait RegularSyncController {
  self: SyncController =>

  private var headersQueue: Seq[BlockHeader] = Seq.empty
  private var resolvingBranch = false
  private var broadcasting = false
  private var timeout: Option[Cancellable] = None

  import Config.FastSync._

  def regularSync(): Receive = {

    case StartSyncing =>
      askForHeaders()

    case ResumeRegularSync =>
      askForHeaders()

    case MessageReceived(m: BlockHeaders) if !resolvingBranch =>
      cancelTimeout()
      handleDownload(m)

    case MessageReceived(m: BlockHeaders) if resolvingBranch =>
      cancelTimeout()
      handleBlockBranchResolution(m)

    case MessageReceived(m: BlockBodies) =>
      cancelTimeout()
      handleBlockBodies(m)

    case m: BroadcastBlocks if broadcasting =>
      //FIXME: Decide block propagation algorithm (for now we send block to every peer) [EC-87]
      peersToDownloadFrom.keys.foreach(_ ! m)

    case ResolveBranch(peer) =>
      requestHeadersForNewBranch(peer)

    case PeerTimeOut(peer) =>
      blacklist(peer, blacklistDuration)
      resolvingBranch = false
      askForHeaders()
  }

  private def requestHeadersForNewBranch(peer: ActorRef) = {
    resolvingBranch = true
    sendWithTimeout(peer, SendMessage(GetBlockHeaders(Right(headersQueue.head.parentHash), blockResolveDepth, skip = 0, reverse = true)))
  }

  private def askForHeaders() = {
    bestPeer match {
      case Some(peer) =>
        val blockNumber = appStateStorage.getBestBlockNumber()
        peer ! Subscribe(Set(BlockHeaders.code, BlockBodies.code))
        sendWithTimeout(peer, SendMessage(GetBlockHeaders(Left(blockNumber + 1), blockHeadersPerRequest, skip = 0, reverse = false)))
      case None =>
        log.warning("no peers to download from")
        scheduleResume()
    }
  }

  private def handleBlockBranchResolution(message: BlockHeaders) =
    //todo limit max branch depth?
    if (message.headers.nonEmpty && message.headers.head.hash == headersQueue.head.parentHash) {
      headersQueue = message.headers.reverse ++ headersQueue
      processBlockHeaders(headersQueue, sender())
    } else {
      //we did not get previous blocks, there is no way to resolve, blacklist peer and continue download
      resumeWithDifferentPeer()
    }

  private def handleDownload(message: BlockHeaders) = if (message.headers.nonEmpty) {
    headersQueue = message.headers
    processBlockHeaders(headersQueue, sender())
  } else {
    //no new headers to process, schedule to ask again in future, we are at the top of chain
    broadcasting = true
    scheduleResume()
  }

  private def processBlockHeaders(headers: Seq[BlockHeader], peer: ActorRef) = {
    val parentByNumber = blockchain.getBlockHeaderByNumber(headers.head.number - 1)

    parentByNumber match {
      case Some(parent) if checkHeaders(headers) =>
        //we have same chain
        if (parent.hash == headers.head.parentHash) {
          sendWithTimeout(peer, SendMessage(GetBlockBodies(headers.take(blockBodiesPerRequest).map(_.hash))))
        } else {
          self ! ResolveBranch(peer)
        }
      case _ =>
        log.warning("got header that does not have parent")
        resumeWithDifferentPeer()
    }
  }

  private def handleBlockBodies(m: BlockBodies) = {
    if (m.bodies.nonEmpty) {
      val result = headersQueue.zip(m.bodies).map { case (h, b) => blockValidator(h, b) }

      if (!result.exists(_.isLeft)) {
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

            self ! BroadcastBlocks(newBlocks)
            log.info(s"got new blocks up till block: ${newBlocks.last.block.header.number} " +
              s"with hash ${Hex.toHexString(newBlocks.last.block.header.hash.toArray[Byte])}")
          case None =>
            log.error("no total difficulty for latest block")
        }

        headersQueue = headersQueue.drop(result.length)
        if (headersQueue.nonEmpty) {
          sender() ! SendMessage(GetBlockBodies(headersQueue.take(blockBodiesPerRequest).map(_.hash)))
        } else {
          resolvingBranch = false
          self ! ResumeRegularSync
        }
      } else {
        //blacklist for errors in blocks
        resumeWithDifferentPeer()
      }
    } else {
      //we got empty response for bodies from peer but we got block headers earlier
      resumeWithDifferentPeer()
    }
  }

  private def sendWithTimeout(peer: ActorRef, m: SendMessage[_]) = {
    peer ! m
    timeout = Some(scheduler.scheduleOnce(peerResponseTimeout, self, PeerTimeOut(peer)))
  }

  private def cancelTimeout() = {
    timeout.foreach(_.cancel())
    timeout = None
  }

  private def scheduleResume() = {
    headersQueue = Seq.empty
    scheduler.scheduleOnce(checkForNewBlockInterval, self, ResumeRegularSync)
  }

  private def resumeWithDifferentPeer() = {
    blacklist(sender(), blacklistDuration)
    headersQueue = Seq.empty
    resolvingBranch = false
    self ! ResumeRegularSync
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
  private case class PeerTimeOut(peer:ActorRef)
  private case class ResolveBranch(peer: ActorRef)

  case object StartSyncing
}
