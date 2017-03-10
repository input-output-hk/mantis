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

  private var headers: Seq[BlockHeader] = Seq.empty
  private var resolvingBranch = false
  private var broadcasting = false

  import Config.FastSync._

  def regularSync(): Receive = {

    case StartRegularSync =>
      askForHeaders()

    case ResumeRegularSync =>
      askForHeaders()

    case MessageReceived(m: BlockHeaders) if !resolvingBranch =>
      handleDownload(m)

    case MessageReceived(m: BlockHeaders) if resolvingBranch =>
      handleBlockBranchResolution(m)

    case MessageReceived(m: BlockBodies) =>
      handleBlockBodies(m)

    case m: BroadcastBlocks if broadcasting =>
      peersToDownloadFrom.keys.foreach(_ ! m)

    case ResolveBranch(peer) =>
      requestHeadersForNewBranch(peer)
  }

  private def requestHeadersForNewBranch(peer: ActorRef) = {
    resolvingBranch = true
    peer ! SendMessage(GetBlockHeaders(Right(headers.head.parentHash), blockResolveDepth, skip = 0, reverse = true))
  }

  private def askForHeaders() = {

    bestPeer match {
      case Some(peer) =>
        val blockNumber = appStateStorage.getBestBlockNumber()

        peer ! Subscribe(Set(BlockHeaders.code, BlockBodies.code))
        peer ! SendMessage(GetBlockHeaders(Left(blockNumber + 1), blockHeadersPerRequest, skip = 0, reverse = false))
      case None =>
        log.warning("no peers to download from")
        scheduleResume()
    }
  }

  private def handleBlockBranchResolution(message: BlockHeaders) =
    //todo limit max branch depth?
    if (message.headers.nonEmpty && message.headers.head.hash == headers.head.parentHash) {
      headers = message.headers.reverse ++ headers
      processBlockHeaders(headers, sender())
    } else {
      //we did not get previous blocks, there is no way to resolve, blacklist peer and continue download
      resumeWithDifferentPeer()
    }

  private def handleDownload(message: BlockHeaders) = if (message.headers.nonEmpty) {
    headers = message.headers
    processBlockHeaders(headers, sender())
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
          peer ! SendMessage(GetBlockBodies(headers.take(blockBodiesPerRequest).map(_.hash)))
        } else {
          context.self ! ResolveBranch(sender())
        }
      case _ =>
        log.warning("got header that does not have parent")
        resumeWithDifferentPeer()
    }
  }

  private def handleBlockBodies(m: BlockBodies) = {
    if (m.bodies.nonEmpty) {
      val result = headers.zip(m.bodies).map { case (h, b) => blockValidator(h, b) }

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
              (blockHashToDelete, NewBlock(b, currentTd))
            }

            newBlocks.collect { case (Some(hash), _) => hash }.foreach(blockchain.removeBlock)
            context.self ! BroadcastBlocks(newBlocks.map(_._2))
            log.info(s"got new blocks up till block: ${newBlocks.last._2.block.header.number} " +
              s"with hash ${Hex.toHexString(newBlocks.last._2.block.header.hash.toArray[Byte])}")
          case None =>
            log.error("no total difficulty for latest block")
        }

        headers = headers.drop(result.length)
        if (headers.nonEmpty) {
          sender() ! SendMessage(GetBlockBodies(headers.take(blockBodiesPerRequest).map(_.hash)))
        } else {
          resolvingBranch = false
          context.self ! ResumeRegularSync
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

  private def scheduleResume() = {
    headers = Seq.empty
    scheduler.scheduleOnce(checkForNewBlockInterval, context.self, ResumeRegularSync)
  }

  private def resumeWithDifferentPeer() = {
    blacklist(sender(), blacklistDuration)
    headers = Seq.empty
    context.self ! ResumeRegularSync
  }

  private def checkHeaders(headers: Seq[BlockHeader]): Boolean =
    headers.zip(headers.tail).forall { case (parent, child) => parent.hash == child.parentHash && parent.number + 1 == child.number }

  private def bestPeer: Option[ActorRef] = Try(peersToDownloadFrom.maxBy {
    case (_, Handshaked(_, Chain.ETC, totalDifficulty)) => totalDifficulty
  }._1).toOption

  private case object ResumeRegularSync

  private case class ResolveBranch(peer: ActorRef)

}
