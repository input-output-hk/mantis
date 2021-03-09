package io.iohk.ethereum.ommers

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props
import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.ommers.OmmersPool.AddOmmers
import io.iohk.ethereum.ommers.OmmersPool.GetOmmers
import org.bouncycastle.util.encoders.Hex

import scala.annotation.tailrec

class OmmersPool(blockchain: Blockchain, ommersPoolSize: Int, ommerGenerationLimit: Int, returnedOmmersSizeLimit: Int)
    extends Actor
    with ActorLogging {

  var ommersPool: Seq[BlockHeader] = Nil

  override def receive: Receive = {
    case AddOmmers(ommers) =>
      ommersPool = (ommers ++ ommersPool).take(ommersPoolSize).distinct
      logStatus(event = "Ommers after add", ommers = ommersPool)

    case GetOmmers(parentBlockHash) =>
      val ancestors = collectAncestors(parentBlockHash, ommerGenerationLimit)
      val ommers = ommersPool
        .filter { b =>
          val notAncestor = ancestors.find(_.hash == b.hash).isEmpty
          ancestors.find(_.hash == b.parentHash).isDefined && notAncestor
        }
        .take(returnedOmmersSizeLimit)
      logStatus(event = s"Ommers given parent block ${Hex.toHexString(parentBlockHash.toArray)}", ommers)
      sender() ! OmmersPool.Ommers(ommers)
  }

  private def collectAncestors(parentHash: ByteString, generationLimit: Int): List[BlockHeader] = {
    @tailrec
    def rec(hash: ByteString, limit: Int, acc: List[BlockHeader]): List[BlockHeader] =
      if (limit > 0) {
        blockchain.getBlockHeaderByHash(hash) match {
          case Some(bh) => rec(bh.parentHash, limit - 1, acc :+ bh)
          case None     => acc
        }
      } else {
        acc
      }
    rec(parentHash, generationLimit, List.empty)
  }

  private def logStatus(event: String, ommers: Seq[BlockHeader]): Unit = {
    lazy val ommersAsString: Seq[String] = ommers.map(bh => s"[number = ${bh.number}, hash = ${bh.hashAsHexString}]")
    log.debug(s"$event ${ommersAsString}")
  }
}

object OmmersPool {

  /** As is stated on section 11.1, eq. (143) of the YP
    *
    * @param ommerGenerationLimit should be === 6
    * @param returnedOmmersSizeLimit should be === 2
    *
    * ^ Probably not worthy but those params could be placed in consensus config.
    */
  def props(
      blockchain: Blockchain,
      ommersPoolSize: Int,
      ommerGenerationLimit: Int = 6,
      returnedOmmersSizeLimit: Int = 2
  ): Props = Props(
    new OmmersPool(blockchain, ommersPoolSize, ommerGenerationLimit, returnedOmmersSizeLimit)
  )

  case class AddOmmers(ommers: List[BlockHeader])

  object AddOmmers {
    def apply(b: BlockHeader*): AddOmmers = AddOmmers(b.toList)
  }

  case class GetOmmers(parentBlockHash: ByteString)

  case class Ommers(headers: Seq[BlockHeader])
}
