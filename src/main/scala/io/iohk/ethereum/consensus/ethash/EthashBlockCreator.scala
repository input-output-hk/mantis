package io.iohk.ethereum.consensus.ethash

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.{Timeout, ByteString}
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

  lazy val fullConsensusConfig = consensus.config
  private lazy val consensusConfig = fullConsensusConfig.generic
  lazy val miningConfig = fullConsensusConfig.specific
  private lazy val coinbase: Address = consensusConfig.coinbase
  private lazy val blockGenerator: EthashBlockGenerator = consensus.blockGenerator

  def getBlockForMining(parentBlock: Block, withTransactions: Boolean = true): Future[PendingBlock] = {
    val transactions =
      if (withTransactions) getTransactionsFromPool else Future.successful(PendingTransactionsResponse(Nil))
    getOmmersFromPool(parentBlock.hash).zip(transactions).flatMap { case (ommers, pendingTxs) =>
      val pendingBlock = blockGenerator
        .generateBlock(parentBlock, pendingTxs.pendingTransactions.map(_.stx.tx), coinbase, ommers.headers)
      Future.successful(pendingBlock)
    }
  }

  private def getOmmersFromPool(parentBlockHash: ByteString): Future[OmmersPool.Ommers] = {
    (ommersPool ? OmmersPool.GetOmmers(parentBlockHash))(Timeout(miningConfig.ommerPoolQueryTimeout))
      .mapTo[OmmersPool.Ommers]
      .recover { case ex =>
        log.error("Failed to get ommers, mining block with empty ommers list", ex)
        OmmersPool.Ommers(Nil)
      }
  }

}
