package io.iohk.ethereum.forking

import cats.implicits._
import io.iohk.ethereum.forking.RpcClient.RpcResponseOps
import monix.eval.Task
import monix.reactive.Observable
import ShowInstances._
import akka.util.ByteString
import cats.Show
import io.iohk.ethereum.forking.EthRpcClient.BlockResponse
import io.iohk.ethereum.forking.ForkChecker.{BlockCheck, Distribution}
import io.iohk.ethereum.utils.Logger

class ForkChecker(clients: List[EthRpcClient]) extends Logger {
  def const[T](value: T): Any => T = _ => value

  val filterLiveClients: Task[List[EthRpcClient]] =
    clients.filterA(client => client.lastBlock.fold(const(false), const(true)))

  def findCommonStableBlockNumber(liveClients: List[EthRpcClient]): Task[BigInt] = liveClients
    .traverse(_.lastStableBlock.peel)
    .map(_.flattenOption.map(_.number).minimumOption.getOrElse(0))

  val findFork: Task[List[BlockCheck]] = {
    for {
      liveClients <- filterLiveClients
      _ <- Task { log.info(show"Live clients: ${liveClients.map(_.name).sorted}") }
      commonStableBlockNumber <- findCommonStableBlockNumber(liveClients)
      _ <- Task { log.info(show"Checking for forks up to block ${commonStableBlockNumber}") }
      checks <- Observable
        .fromIterable(BigInt(1) to commonStableBlockNumber)
        .mapEval(nr =>
          liveClients
            .traverse(client => (Task.pure(client.name), client.getBlockByNumber(nr).peel).mapN(_ -> _))
            .map(_.toMap)
            .map(BlockCheck(nr, _))
        )
        .toListL
    } yield checks
  }

  val checkMiningDistribution: Task[Distribution] = {
    for {
      liveClients <- filterLiveClients
      commonStableBlockNumber <- findCommonStableBlockNumber(liveClients)
      clientToUse = liveClients.head
      responses <- (BigInt(1) to commonStableBlockNumber).toList
        .traverse(nr => clientToUse.getBlockByNumber(nr).peel)
        .map(_.flattenOption)
      blocksByMiners = responses.groupBy(_.miner.getOrElse(ByteString("undefined"))).mapValues(_.size)
      totalBlocks = responses.size
    } yield Distribution(totalBlocks, blocksByMiners)
  }
}
object ForkChecker {
  case class BlockCheck(nr: BigInt, responses: Map[String, Option[BlockResponse]]) {
    val gotFork: Boolean = responses.values.toSet.size > 1
    val isConsensus: Boolean = !gotFork

    val message: String = if (gotFork) {
      val lines = responses.toList
        .map { case (node, response) => show"$node: ${response.fold("null")(_.hash.show)}" }
        .mkString("\n\t", "\n\t", "\n")
      show"❎ Block ${nr}\t: got fork:" ++ lines
    } else {
      val consentHash: String = responses.values.head.map(_.hash.show).getOrElse("null")
      show"✅ Block ${nr}\t: is consensus on ${consentHash}"
    }
  }
  object BlockCheck {
    implicit val blockCheckShow: Show[BlockCheck] = _.message
    implicit val multipleBlockChecksShow: Show[List[BlockCheck]] = _.map(_.show).mkString("\n")
  }

  case class Distribution(total: Int, distribution: Map[ByteString, Int])
  object Distribution {
    implicit val distributionShow: Show[Distribution] = d => {
      val lines = d.distribution.map { case (miner, nrOfBlocks) => show"${miner}\t: ${nrOfBlocks}/${d.total}" }
      "Mining Distribution:" ++ lines.mkString("\n\t", "\n\t", "\n")
    }
  }
}
