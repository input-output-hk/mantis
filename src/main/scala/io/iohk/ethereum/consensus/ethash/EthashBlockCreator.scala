package io.iohk.ethereum.consensus.ethash

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import io.iohk.ethereum.consensus.blocks.PendingBlock
import io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator
import io.iohk.ethereum.domain.{Address, Block}
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

class EthashBlockCreator(
  val pendingTransactionsManager: ActorRef,
  val getTransactionFromPoolTimeout: FiniteDuration,
  consensus: EthashConsensus,
  ommersPool: ActorRef
) extends TransactionPicker {

  val fullConsensusConfig = consensus.config
  private val consensusConfig = fullConsensusConfig.generic
  val miningConfig = fullConsensusConfig.specific
  private val coinbase: Address = consensusConfig.coinbase
  private val blockGenerator: EthashBlockGenerator = consensus.blockGenerator

  def getBlockForMining(parentBlock: Block, withTransactions: Boolean = true): Future[PendingBlock] = {
    val transactions = if(withTransactions) getTransactionsFromPool else Future.successful(PendingTransactionsResponse(Nil))
    getOmmersFromPool(parentBlock.header.number + 1).zip(transactions).flatMap { case (ommers, pendingTxs) =>
      blockGenerator.generateBlock(parentBlock, pendingTxs.pendingTransactions.map(_.stx.tx), coinbase, ommers.headers) match {
        case Right(pb) => Future.successful(pb)
        case Left(err) => Future.failed(new RuntimeException(s"Error while generating block for mining: $err"))
      }
    }
  }

  private def getOmmersFromPool(blockNumber: BigInt): Future[OmmersPool.Ommers] = {
    (ommersPool ? OmmersPool.GetOmmers(blockNumber))(Timeout(miningConfig.ommerPoolQueryTimeout)).mapTo[OmmersPool.Ommers]
      .recover { case ex =>
        log.error("Failed to get ommers, mining block with empty ommers list", ex)
        OmmersPool.Ommers(Nil)
      }
  }

}
