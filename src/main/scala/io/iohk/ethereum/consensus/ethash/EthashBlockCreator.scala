package io.iohk.ethereum.consensus.ethash

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator
import io.iohk.ethereum.domain.{Address, Block}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps.TaskActorOps
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.transactions.TransactionPicker
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

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
  lazy val blockchainConfig = consensus.blockchainConfig

  def getBlockForMining(
      parentBlock: Option[Block],
      withTransactions: Boolean = true,
      initialWorldStateBeforeExecution: Option[InMemoryWorldStateProxy] = None
  ): Task[PendingBlockAndState] = {
    val transactions = if (withTransactions) getTransactionsFromPool else Task.now(PendingTransactionsResponse(Nil))
    parentBlock match {
      case Some(parent) =>
        Task.parZip2(getOmmersFromPool(parent.hash), transactions).map { case (ommers, pendingTxs) =>
          blockGenerator.generateBlock(
            parent,
            pendingTxs.pendingTransactions.map(_.stx.tx),
            coinbase,
            ommers.headers,
            initialWorldStateBeforeExecution
          )
        }
      case None =>
        Task.raiseError(new RuntimeException("error reading block"))
    }
  }

  private def getOmmersFromPool(parentBlockHash: ByteString): Task[OmmersPool.Ommers] = {
    ommersPool
      .askFor[OmmersPool.Ommers](OmmersPool.GetOmmers(parentBlockHash))
      .onErrorHandle { ex =>
        log.error("Failed to get ommers, mining block with empty ommers list", ex)
        OmmersPool.Ommers(Nil)
      }
  }

}
