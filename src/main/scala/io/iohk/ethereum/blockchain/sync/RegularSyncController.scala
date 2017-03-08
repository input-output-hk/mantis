package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorContext, ActorRef, Scheduler}
import akka.event.LoggingAdapter
import io.iohk.ethereum.blockchain.sync.FastSyncController.StartRegularSync
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.network.PeerActor.{BroadcastBlocks, MessageReceived, SendMessage, Subscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.validators.BlockValidator
import io.iohk.ethereum.utils.Config

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

trait RegularSyncController {

  val blockchain: Blockchain
  val context: ActorContext

  def blacklist(peer: ActorRef, duration: FiniteDuration)

  def peersToDownloadFrom: Map[ActorRef, Status]

  def scheduler: Scheduler

  private var headers: Seq[BlockHeader] = Seq.empty
  private var totalDifficulty: Option[BigInt] = None
  private var resolvingBranche = false
  //todo replace with AppStateStorage from feature/resumeStateDownload
  private val maxBlockNumber = 42


  import Config.FastSync._

  def regularSync(log: LoggingAdapter): Actor.Receive = {

    case StartRegularSync =>
      peersToDownloadFrom.foreach { case (ref, _) =>
        ref ! Subscribe(Set(BlockHeaders.code, BlockBodies.code))
      }
      askForHeaders(log)

    case ResumeRegularSync =>
      askForHeaders(log)

    case MessageReceived(m: BlockHeaders) if !resolvingBranche =>
      handleDownload(log, m)

    case MessageReceived(m: BlockHeaders) if resolvingBranche =>
      handleBlockBranchResolution(log, m)

    case MessageReceived(m: BlockBodies) =>
      handleBlockBodies(m)

    //todo move to separate actor to use also in NewBlock broadcast
    case ResolveBranch(peer) =>
      val TODOnumberFromConfig = 100
      resolvingBranche = true
      peer ! SendMessage(GetBlockHeaders(Right(headers.head.parentHash), TODOnumberFromConfig, skip = 0, reverse = true))
  }

  private def askForHeaders(log: LoggingAdapter) = {
    bestPeer match {
      case Some(peer) =>
        peer ! SendMessage(GetBlockHeaders(Left(maxBlockNumber + 1), blockHeadersPerRequest, skip = 0, reverse = false))
      case None =>
        log.warning("no peers to download from")
        scheduleResume
    }
  }

  private def handleBlockBranchResolution(log: LoggingAdapter, message: BlockHeaders) =
    if (message.headers.nonEmpty && message.headers.head.hash == headers.head.parentHash) {
      headers = message.headers.reverse ++ headers
      processBlockHeaders(headers, context.sender(), log)
    } else {
      //we did not get previous blocks, there is no way to resolve, blacklist peer and continue download
      scheduleResume
      blacklist(context.sender(), blacklistDuration)
    }

  private def handleDownload(log: LoggingAdapter, message: BlockHeaders) = if (message.headers.nonEmpty) {
    headers = message.headers.sortBy(_.number)
    processBlockHeaders(headers, context.sender(), log)
  } else {
    //no new headers to process
    //TODO should we stop regular sync and swith to block broadcast?
    scheduleResume
  }

  private def processBlockHeaders(headers: Seq[BlockHeader], peer: ActorRef, log: LoggingAdapter) = {
    val parentByNumber = blockchain.getBlockHeaderByNumber(headers.head.number - 1)

    parentByNumber match {
      case Some(parent) =>
        //we have same chain
        if (parent.hash == headers.head.parentHash)
          peer ! SendMessage(GetBlockBodies(headers.take(blockBodiesPerRequest).map(_.hash)))
        else
          context.self ! ResolveBranch(context.sender())
      case None =>
        scheduleResume
        blacklist(context.sender(), blacklistDuration)
        log.warning("got header that does not have parent")
    }
  }

  private def handleBlockBodies(m: BlockBodies) = {
    if (m.bodies.nonEmpty) {
      val result = headers.zip(m.bodies).map { case (h, b) => BlockValidator.validateHeaderAndBody(h, b) }

      if (!result.exists(_.isLeft)) {
        val blocks = result.collect { case Right(b) => b }

        blockchain.getTotalDifficultyByHash(blocks.head.header.parentHash)

        val newBlocks = blocks.flatMap { b =>
          blockchain.save(b)
          totalDifficulty = totalDifficulty.map(_ + b.header.difficulty)
          totalDifficulty.map(td => NewBlock(b, td))
        }

        peersToDownloadFrom.keys.foreach {
          _ ! BroadcastBlocks(newBlocks)
        }
      } else {
        headers == Seq.empty
        scheduleResume
      }
    } else {
      //we got empty response from peer
      blacklist(context.sender(), blacklistDuration)
      headers == Seq.empty
      scheduleResume
    }
  }

  private def scheduleResume = {
    //todo configure that 16 seconds
    headers = Seq.empty
    scheduler.scheduleOnce(16.seconds, context.self, ResumeRegularSync)
  }

  private def bestPeer: Option[ActorRef] = Try(peersToDownloadFrom.maxBy { case (_, status) => status.totalDifficulty }._1).toOption

  private case object ResumeRegularSync

  private case class ResolveBranch(peer: ActorRef)

}
