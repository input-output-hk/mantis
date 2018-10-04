package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.metrics.{ Metrics, MetricsClient }
import io.iohk.ethereum.utils.{ BlockchainConfig, Logger }
import org.bouncycastle.util.encoders.Hex

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future }

class BlockImport(
  blockchain: BlockchainImpl,
  blockQueue: BlockQueue,
  blockchainConfig: BlockchainConfig,
  executeBlock: (Block, Boolean) => Either[BlockExecutionError, Seq[Receipt]],
  blockValidation: BlockValidation,
  validationContext: ExecutionContext // Can't be implicit because of importToTop method and ambiguous of executionContext
) extends Logger {

  private[ledger] def importToTop(
    block: Block,
    currentBestBlock: Block,
    currentTd: BigInt
  )(implicit blockExecutionContext: ExecutionContext): Future[BlockImportResult] = {
    val validationResult = Future(blockValidation.validateBlockBeforeExecution(block))(validationContext)
    val importResult = Future(importBlockToTop(block, currentBestBlock.header.number, currentTd))(blockExecutionContext)

    for {
      validationResult <- validationResult
      importResult     <- importResult
    } yield {
      validationResult.fold(error => handleImportTopValidationError(error, block, currentBestBlock, importResult), _ => importResult)
    }
  }

  private def importBlockToTop(block: Block, bestBlockNumber: BigInt, currentTd: BigInt): BlockImportResult = {
    val topBlockHash = blockQueue.enqueueBlock(block, bestBlockNumber).get.hash
    val topBlocks = blockQueue.getBranch(topBlockHash, dequeue = true)
    val (importedBlocks, maybeError) = executeBlocks(topBlocks, currentTd)

    val result = maybeError match {
      case None =>
        BlockImportedToTop(importedBlocks)

      case Some(error) if importedBlocks.isEmpty =>
        blockQueue.removeSubtree(block.header.hash)
        BlockImportFailed(error.toString)

      case Some(_) =>
        topBlocks.drop(importedBlocks.length).headOption.foreach { failedBlock =>
          blockQueue.removeSubtree(failedBlock.header.hash)
        }
        BlockImportedToTop(importedBlocks)
    }

    log.debug("{}", {
      val result = importedBlocks.map { blockData =>
        val header = blockData.block.header
        s"Imported new block (${header.number}: ${Hex.toHexString(header.hash.toArray)}) to the top of chain \n"
      }
      result.toString
    })

    if(importedBlocks.nonEmpty) {
      val maxNumber = importedBlocks.map(_.block.header.number).max
      MetricsClient.get().gauge(Metrics.LedgerImportBlockNumber, maxNumber.toLong)
    }

    result
  }

  /** Executes a list blocks, storing the results in the blockchain.
    *
    * @param blocks   blocks to be executed
    * @param parentTd transaction difficulty of the parent
    *
    * @return a list of blocks that were correctly executed and an optional [[BlockExecutionError]]
    */
  private def executeBlocks(blocks: List[Block], parentTd: BigInt): (List[BlockData], Option[BlockExecutionError]) = {
    @tailrec
    def go(executedBlocks: List[BlockData], remainingBlocks: List[Block], parentTd: BigInt, error: Option[BlockExecutionError])
    :(List[BlockData], Option[BlockExecutionError]) ={
      if (remainingBlocks.isEmpty) {
        (executedBlocks.reverse, None)
      } else if (error.isDefined) {
        (executedBlocks, error)
      } else {
        val blockToExecute = remainingBlocks.head
        executeBlock(blockToExecute, true) match {
          case Right (receipts) =>
            val td = parentTd + blockToExecute.header.difficulty
            val newBlockData = BlockData(blockToExecute, receipts, td)
            blockchain.save(newBlockData.block, newBlockData.receipts, newBlockData.td, saveAsBestBlock = true)
            go(newBlockData :: executedBlocks, remainingBlocks.tail, td, None)
          case Left(executionError) =>
            go(executedBlocks, remainingBlocks, 0, Some(executionError))
        }
      }
    }

    go(List.empty[BlockData], blocks, parentTd, None)
  }

  private def handleImportTopValidationError(
    error: ValidationBeforeExecError,
    block: Block,
    bestBlockBeforeImport: Block,
    blockImportResult: BlockImportResult
  ): BlockImportResult = {
    blockImportResult match {
      case BlockImportedToTop(blockImportData) =>
        blockImportData.foreach { blockData =>
          val hash = blockData.block.header.hash
          blockQueue.removeSubtree(hash)
          blockchain.removeBlock(hash, withState = true)
        }
        blockchain.saveBestKnownBlock(bestBlockBeforeImport.header.number)
      case _ => ()
    }
    handleBlockValidationError(error, block)
  }

  private def handleBlockValidationError(error: ValidationBeforeExecError, block: Block): BlockImportResult = error match {
    case ValidationBeforeExecError(HeaderParentNotFoundError) =>
      log.debug(s"Block(${block.idTag}) has unknown parent")
      UnknownParent

    case ValidationBeforeExecError(reason) =>
      log.debug(s"Block(${block.idTag}) failed pre-import validation")
      BlockImportFailed(reason.toString)
  }

  private[ledger] def reorganise(block: Block, currentBestBlock: Block, currentTd: BigInt): Future[BlockImportResult] = Future {
    blockValidation.validateBlockBeforeExecution(block).fold(error => handleBlockValidationError(error, block), _ => {
      blockQueue.enqueueBlock(block, currentBestBlock.header.number) match {
        case Some(Leaf(leafHash, leafTd)) if isBetterBranch(block, currentBestBlock, leafTd, currentTd) =>
          log.debug("Found a better chain, about to reorganise")
          reorganiseChain(leafHash, leafTd)

        case _ =>
          BlockEnqueued
      }
    })
  }(validationContext)

  private def isBetterBranch(block: Block, bestBlock: Block, newTd: BigInt, currentTd: BigInt): Boolean =
    newTd > currentTd || (blockchainConfig.gasTieBreaker && newTd == currentTd && block.header.gasUsed > bestBlock.header.gasUsed)

  private def reorganiseChain(leafHash: ByteString, leafTd: BigInt): BlockImportResult = {
    reorganiseChainFromQueue(leafHash) match {
      case Right((oldBranch, newBranch)) =>
        val totalDifficulties = newBranch.tail.foldRight(List(leafTd)) { (b, tds) =>
          (tds.head - b.header.difficulty) :: tds
        }
        ChainReorganised(oldBranch, newBranch, totalDifficulties)

      case Left(error) =>
        BlockImportFailed(s"Error while trying to reorganise chain: $error")
    }
  }

  /** Once a better branch was found this attempts to reorganise the chain
    *
    * @param queuedLeaf a block hash that determines a new branch stored in the queue (newest block from the branch)
    * @return [[BlockExecutionError]] if one of the blocks in the new branch failed to execute, otherwise:
    *        (oldBranch, newBranch) as lists of blocks
    */
  private def reorganiseChainFromQueue(queuedLeaf: ByteString): Either[BlockExecutionError, (List[Block], List[Block])] = {
    blockchain.persistCachedNodes()
    val newBranch = blockQueue.getBranch(queuedLeaf, dequeue = true)
    val bestNumber = blockchain.getBestBlockNumber()

    val parent = newBranch.head.header.parentHash
    val parentTd = blockchain.getTotalDifficultyByHash(parent).get

    val staleBlocksWithReceiptsAndTDs = removeBlocksUntil(parent, bestNumber).reverse
    val staleBlocks = staleBlocksWithReceiptsAndTDs.map(_.block)

    for (block <- staleBlocks) yield blockQueue.enqueueBlock(block)

    val (executedBlocks, maybeError) = executeBlocks(newBranch, parentTd)
    maybeError match {
      case None =>
        Right(staleBlocks, executedBlocks.map(_.block))

      case Some(error) =>
        revertChainReorganisation(newBranch, staleBlocksWithReceiptsAndTDs, executedBlocks)
        Left(error)
    }
  }

  /** Reverts chain reorganisation in the event that one of the blocks from new branch fails to execute
    *
    * @param newBranch      new blocks
    * @param oldBranch      old blocks along with corresponding receipts and totalDifficulties
    * @param executedBlocks sub-sequence of new branch that was executed correctly
    */
  private def revertChainReorganisation(newBranch: List[Block], oldBranch: List[BlockData], executedBlocks: List[BlockData]): Unit = {
    if (executedBlocks.nonEmpty) {
      removeBlocksUntil(executedBlocks.head.block.header.parentHash, executedBlocks.last.block.header.number)
    }

    oldBranch.foreach { case BlockData(block, receipts, td) =>
      blockchain.save(block,receipts, td, saveAsBestBlock = false)
    }

    val bestNumber = oldBranch.last.block.header.number
    blockchain.saveBestKnownBlock(bestNumber)
    executedBlocks.foreach(data => blockQueue.enqueueBlock(data.block, bestNumber))

    newBranch.diff(executedBlocks.map(_.block)).headOption.foreach { block =>
      blockQueue.removeSubtree(block.header.hash)
    }
  }

  /** Removes blocks from the [[Blockchain]] along with receipts and total difficulties.
    *
    * @param parent     remove blocks until this hash (exclusive)
    * @param fromNumber start removing from this number (downwards)
    *
    * @return the list of removed blocks along with receipts and total difficulties
    */
  private def removeBlocksUntil(parent: ByteString, fromNumber: BigInt): List[BlockData] = {
    blockchain.getBlockByNumber(fromNumber) match {
      case Some(block) if block.header.hash == parent =>
        Nil

      case Some(block) =>
        val hash = block.header.hash

        val blockList = for {
          receipts <- blockchain.getReceiptsByHash(hash)
          td       <- blockchain.getTotalDifficultyByHash(hash)
        } yield BlockData(block, receipts, td):: removeBlocksUntil(parent, fromNumber - 1)

        // Not updating best block number for efficiency, it will be updated in the callers anyway
        blockchain.removeBlock(hash, withState = true)

        blockList.getOrElse(Nil)

      case None =>
        log.error(s"Unexpected missing block number: $fromNumber")
        Nil
    }
  }
}

sealed trait BlockImportResult

case class BlockImportedToTop(blockImportData: List[BlockData]) extends BlockImportResult

case object BlockEnqueued extends BlockImportResult

case object DuplicateBlock extends BlockImportResult

case class ChainReorganised(oldBranch: List[Block], newBranch: List[Block], totalDifficulties: List[BigInt]) extends BlockImportResult

case class BlockImportFailed(error: String) extends BlockImportResult

case object UnknownParent extends BlockImportResult
