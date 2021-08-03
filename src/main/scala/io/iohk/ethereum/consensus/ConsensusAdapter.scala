package io.iohk.ethereum.consensus

import io.iohk.ethereum.blockchain.sync.regular.BlockImportFailed
import io.iohk.ethereum.blockchain.sync.regular.DuplicateBlock
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.BlockExecutionSuccess
import io.iohk.ethereum.ledger.BlockQueue
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.utils.FunctorOps._
import io.iohk.ethereum.utils.Hex
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.blockchain.sync.regular.BlockImportResult
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.utils.BlockchainConfig
import monix.eval.Task
import monix.execution.Scheduler

/** This is a temporary class to isolate the real Consensus and extract responsibilities which should not
  * be part of the consensus in the final design, but are currently needed.
  */
class ConsensusAdapter(
    consensus: Consensus,
    blockchainReader: BlockchainReader,
    blockQueue: BlockQueue,
    blockValidation: BlockValidation,
    validationScheduler: Scheduler
) extends Logger {
  def evaluateBranchBlock(
      block: Block
  )(implicit blockExecutionScheduler: Scheduler, blockchainConfig: BlockchainConfig): Task[BlockImportResult] =
    blockchainReader.getBestBlock() match {
      case Some(bestBlock) =>
        if (isBlockADuplicate(block.header, bestBlock.header.number)) {
          log.debug("Ignoring duplicated block: {}", block.idTag)
          Task.now(DuplicateBlock)
        } else {
          doBlockPreValidation(block).flatMap {
            case Left(error) =>
              Task.now(BlockImportFailed(error.reason.toString))
            case Right(BlockExecutionSuccess) =>
              consensus.evaluateBranch(Seq(block))
          }
        }
      case None =>
        log.error("Couldn't find the current best block")
        Task.now(BlockImportFailed("Couldn't find the current best block"))
    }

  private def doBlockPreValidation(block: Block)(implicit
      blockchainConfig: BlockchainConfig
  ): Task[Either[ValidationBeforeExecError, BlockExecutionSuccess]] =
    Task
      .evalOnce(blockValidation.validateBlockBeforeExecution(block))
      .tap {
        case Left(error) =>
          log.error(
            "Error while validating block with hash {} before execution: {}",
            Hex.toHexString(block.hash.toArray),
            error.reason.toString
          )
        case Right(_) => log.debug("Block with hash {} validated successfully", Hex.toHexString(block.hash.toArray))
      }
      .executeOn(validationScheduler)

  private def isBlockADuplicate(block: BlockHeader, currentBestBlockNumber: BigInt): Boolean = {
    val hash = block.hash
    blockchainReader.getBlockByHash(hash).isDefined &&
    block.number <= currentBestBlockNumber ||
    blockQueue.isQueued(hash)
  }
}
