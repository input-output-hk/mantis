package io.iohk.ethereum.ommers

import akka.actor.{Actor, Props}
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.ommers.OmmersPool.{AddOmmers, GetOmmers, RemoveOmmers}
import io.iohk.ethereum.utils.MiningConfig

class OmmersPool(blockchain: Blockchain, miningConfig: MiningConfig) extends Actor {

  var ommersPool: Seq[BlockHeader] = Nil

  val ommerGenerationLimit: Int = 6 //Stated on section 11.1, eq. (143) of the YP
  val ommerSizeLimit: Int = 2

  override def receive: Receive = {
    case AddOmmers(ommers) =>
      ommersPool = (ommers ++ ommersPool).take(miningConfig.ommersPoolSize).distinct

    case RemoveOmmers(ommers) =>
      val toDelete = ommers.map(_.hash).toSet
      ommersPool = ommersPool.filter(b => !toDelete.contains(b.hash))

    case GetOmmers(blockNumber) =>
      ommersPool.filter { b =>
        val generationDifference = blockNumber - b.number
        generationDifference > 0 && generationDifference <= ommerGenerationLimit
      }.filter { b =>
        blockchain.getBlockHeaderByHash(b.parentHash).isDefined
      }.take(ommerSizeLimit)
  }
}

object OmmersPool {
  def props(blockchain: Blockchain, miningConfig: MiningConfig): Props = Props(new OmmersPool(blockchain, miningConfig))

  case class AddOmmers(ommers: List[BlockHeader])

  object AddOmmers {
    def apply(b: BlockHeader*): AddOmmers = AddOmmers(b.toList)
  }

  case class RemoveOmmers(ommers: List[BlockHeader])

  object RemoveOmmers {
    def apply(b: BlockHeader*): RemoveOmmers = RemoveOmmers(b.toList)
  }

  case class GetOmmers(blockNumber: BigInt)

  case class Ommers(headers: Seq[BlockHeader])
}
