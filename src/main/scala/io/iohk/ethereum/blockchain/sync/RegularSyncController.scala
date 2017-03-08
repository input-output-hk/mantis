package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorRef, Scheduler}
import io.iohk.ethereum.blockchain.sync.FastSyncController.StartRegularSync
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.network.PeerActor.{BroadcastBlocks, MessageReceived, SendMessage, Subscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.validators.BlockValidator
import io.iohk.ethereum.utils.Config

trait RegularSyncController {

  val blockchain: Blockchain

  def handshakedPeers: Map[ActorRef, Status]

  def scheduler: Scheduler

  private var headers: Seq[BlockHeader] = Seq.empty
  private var totalDifficulty: Option[BigInt] = None

  import Config.FastSync._

  def regularSync(maxBlockNumber: BigInt): Actor.Receive = {

    case StartRegularSync =>
      handshakedPeers.foreach { case (ref, _) =>
        ref ! Subscribe(Set(BlockHeaders.code, BlockBodies.code))
      }
      val (bestPeer, _) = handshakedPeers.maxBy { case (_, status) => status.totalDifficulty }
      bestPeer ! SendMessage(GetBlockHeaders(Left(maxBlockNumber + 1), blockHeadersPerRequest, skip = 0, reverse = false))

    case MessageReceived(m: BlockHeaders) =>
      val (bestPeer, _) = handshakedPeers.maxBy { case (_, status) => status.totalDifficulty }
      headers = m.headers.sortBy(_.number)
      bestPeer ! SendMessage(GetBlockBodies(headers.map(_.hash)))


    case MessageReceived(m: BlockBodies) =>
      if (headers.nonEmpty && m.bodies.size == headers.size) {
        val result = headers.zip(m.bodies).map { case (h, b) => BlockValidator.validateHeaderAndBody(h, b) }
        val blocks = result.collect { case Right(b) => b }.sortBy(_.header.number)

        blockchain.getTotalDifficultyByHash(blocks.head.header.parentHash)

        val newBlocks = blocks.flatMap { b =>
          blockchain.save(b)
          totalDifficulty = totalDifficulty.map(_ + b.header.difficulty)
          totalDifficulty.map(td => NewBlock(b, td))
        }

        //blockchain.save(b.header.hash, 42)
        //NewBlock(b,td)

        handshakedPeers.keys.foreach{_ ! BroadcastBlocks(newBlocks)}
      } else {
        //TODO request again
      }
  }
}
