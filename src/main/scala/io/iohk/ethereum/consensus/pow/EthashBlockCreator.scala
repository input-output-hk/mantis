package io.iohk.ethereum.consensus.pow

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.consensus.pow.blocks.PoWBlockGenerator
import io.iohk.ethereum.domain.{Address, Block}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps.TaskActorOps
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.transactions.TransactionPicker
import io.iohk.ethereum.utils.ByteStringUtils.ByteStringOps
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

class EthashBlockCreator(
    val pendingTransactionsManager: ActorRef,
    val getTransactionFromPoolTimeout: FiniteDuration,
    consensus: PoWConsensus,
    ommersPool: ActorRef
) extends TransactionPicker {

  lazy val fullConsensusConfig = consensus.config
  private lazy val consensusConfig = fullConsensusConfig.generic
  lazy val miningConfig = fullConsensusConfig.specific
  private lazy val coinbase: Address = consensusConfig.coinbase
  private lazy val blockGenerator: PoWBlockGenerator = consensus.blockGenerator
  lazy val blockchainConfig = consensus.blockchainConfig

  def getBlockForMining(
      parentBlock: Block,
      withTransactions: Boolean = true,
      initialWorldStateBeforeExecution: Option[InMemoryWorldStateProxy] = None
  ): Task[PendingBlockAndState] = {
    log.debug("Getting block for mining, withTxs: {}", withTransactions)
    val transactions = if (withTransactions) getTransactionsFromPool else Task.now(PendingTransactionsResponse(Nil))
    Task.parZip2(getOmmersFromPool(parentBlock.hash), transactions).map { case (ommers, pendingTxs) =>
      log.debug("Transactions for block {}", pendingTxs.pendingTransactions.map(_.stx.tx.hash.toHex))
      blockGenerator.generateBlock(
        parentBlock,
        pendingTxs.pendingTransactions.map(_.stx.tx),
        coinbase,
        ommers.headers,
        initialWorldStateBeforeExecution
      )
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
