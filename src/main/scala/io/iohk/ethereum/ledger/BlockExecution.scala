package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.TxsExecutionError
import io.iohk.ethereum.ledger.Ledger.BlockResult
import io.iohk.ethereum.utils.{ BlockchainConfig, DaoForkConfig, Logger }
import io.iohk.ethereum.vm.EvmConfig

class BlockExecution(
  blockchain: BlockchainImpl,
  blockchainConfig: BlockchainConfig,
  blockPreparator: BlockPreparator,
  consensus: Consensus
) extends Logger {

  /** This function runs transaction
    *
    * @param block the block with transactions to run
    */
  private[ledger] def executeBlockTransactions(block: Block): Either[BlockExecutionError, BlockResult] = {
    val parentStateRoot = blockchain.getBlockHeaderByHash(block.header.parentHash).map(_.stateRoot)
    val blockHeaderNumber = block.header.number
    val initialWorld =
      blockchain.getWorldStateProxy(
        blockHeaderNumber,
        blockchainConfig.accountStartNonce,
        parentStateRoot,
        EvmConfig.forBlock(blockHeaderNumber, blockchainConfig).noEmptyAccounts,
        ethCompatibleStorage = blockchainConfig.ethCompatibleStorage)

    val inputWorld = blockchainConfig.daoForkConfig match {
      case Some(daoForkConfig) if daoForkConfig.isDaoForkBlock(blockHeaderNumber) => drainDaoForkAccounts(initialWorld, daoForkConfig)
      case _ => initialWorld
    }

    val hashAsHexString = block.header.hashAsHexString
    val transactionList = block.body.transactionList
    log.debug(s"About to execute ${transactionList.size} txs from block $blockHeaderNumber (with hash: $hashAsHexString)")
    val blockTxsExecResult = blockPreparator.executeTransactions(transactionList, inputWorld, block.header)
    blockTxsExecResult match {
      case Right(_) => log.debug(s"All txs from block $hashAsHexString were executed successfully")
      case Left(error) => log.debug(s"Not all txs from block $hashAsHexString were executed correctly, due to ${error.reason}")
    }
    blockTxsExecResult
  }

  /** This function updates worldState transferring balance from drainList accounts to refundContract address
    *
    * @param worldState     initial world state
    * @param daoForkConfig  dao fork configuration with drainList and refundContract config
    * @return updated world state proxy
    */
  private def drainDaoForkAccounts(worldState: InMemoryWorldStateProxy, daoForkConfig: DaoForkConfig): InMemoryWorldStateProxy = {
    daoForkConfig.refundContract match {
      case Some(refundContractAddress) =>
        daoForkConfig.drainList.foldLeft(worldState) { (ws, address) =>
          ws.getAccount(address)
            .map(account => ws.transfer(from = address, to = refundContractAddress, account.balance))
            .getOrElse(ws)
        }
      case None => worldState
    }
  }

  private[ledger] final def executeTransactions(
    signedTransactions: Seq[SignedTransaction],
    world: InMemoryWorldStateProxy,
    blockHeader: BlockHeader,
    acumGas: BigInt = 0,
    acumReceipts: Seq[Receipt] = Nil
  ): Either[TxsExecutionError, BlockResult] =
    blockPreparator.executeTransactions(
      signedTransactions = signedTransactions,
      world = world,
      blockHeader = blockHeader,
      acumGas = acumGas,
      acumReceipts = acumReceipts
    )

  private[ledger] def validateBlockAfterExecution(
    block: Block,
    stateRootHash: ByteString,
    receipts: Seq[Receipt],
    gasUsed: BigInt
  ): Either[BlockExecutionError, BlockExecutionSuccess] = consensus.validators.validateBlockAfterExecution(
    block = block,
    stateRootHash = stateRootHash,
    receipts = receipts,
    gasUsed = gasUsed
  )

  private[ledger] def payBlockReward(block: Block, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
    blockPreparator.payBlockReward(block, worldStateProxy)

}

sealed trait BlockExecutionError{
  val reason: Any
}

sealed trait BlockExecutionSuccess

case object BlockExecutionSuccess extends BlockExecutionSuccess

object BlockExecutionError {
  case class ValidationBeforeExecError(reason: Any) extends BlockExecutionError

  case class StateBeforeFailure(worldState: InMemoryWorldStateProxy, acumGas: BigInt, acumReceipts: Seq[Receipt])

  case class TxsExecutionError(stx: SignedTransaction, stateBeforeError: StateBeforeFailure, reason: String) extends BlockExecutionError

  case class ValidationAfterExecError(reason: String) extends BlockExecutionError
}
