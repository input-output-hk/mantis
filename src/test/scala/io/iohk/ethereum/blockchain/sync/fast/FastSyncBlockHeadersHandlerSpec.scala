package io.iohk.ethereum.blockchain.sync.fast

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.fast.FastSync.{ ImportedLastBlock, LastBlockValidationFailed, ProcessSyncing, UpdateTargetBlock }
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.consensus.validators.BlockHeaderValidator
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import org.scalamock.handlers.CallHandler3

class FastSyncBlockHeadersHandlerSpec extends FastSyncHandlersSetup with FastSyncBlockHeadersHandler {

  "FastSyncBlockHeadersHandler" when {

    "calling handleBlockHeaders" should {
      "blacklist peer if headers not in chain order" in {
        blacklist expects(peer1.id, syncConfig.blacklistDuration, *) once()

        val (resultHandler, resultMsg) = handleBlockHeaders(peer1, headers.reverse, baseHandlerState, discardLastBlocks, blacklist)
        resultHandler shouldBe baseHandlerState
        resultMsg shouldBe ProcessSyncing
      }

      "stop processing headers when parent difficulty not found" in {
        // Process first block
        val parentDifficulty: BigInt = blockHeader1.difficulty + defaultTargetBlock.difficulty
        (blockchain.getTotalDifficultyByHash _).expects(blockHeader1.parentHash).returning(Some(parentDifficulty))

        toMockFunction1[BlockHeader, Unit](blockchain.save(_: BlockHeader)).expects(blockHeader1)
        toMockFunction2[ByteString, BigInt, Unit](blockchain.save(_: ByteString, _: BigInt))
          .expects(hash1, blockHeader1.difficulty + parentDifficulty)

        // Stop processing blocks at second block
        val secondBlock = blockHeader2
        (blockchain.getTotalDifficultyByHash _).expects(secondBlock.parentHash).returning(None)
        (log.debug: (String, Any) => Unit).expects(*, *).returning(()).once()
        val (resultHandler, resultMsg) = handleBlockHeaders(peer1, headers, baseHandlerState, discardLastBlocks, blacklist)
        resultHandler.syncState.receiptsQueue shouldBe blockHeader1ExpectedInQueue
        resultHandler.syncState.blockBodiesQueue shouldBe blockHeader1ExpectedInQueue
        resultMsg shouldBe ProcessSyncing
      }

      "finish processing headers if headers list is empty" in {
        val (resultHandler, resultMsg) = handleBlockHeaders(peer1, Seq.empty, baseHandlerState, discardLastBlocks, blacklist)
        resultHandler shouldBe baseHandlerState
        resultMsg shouldBe ProcessSyncing
      }

      "update target block when imported target block" in {
        val parentDifficulty: BigInt = blockHeader1.difficulty + defaultTargetBlock.difficulty
        (blockchain.getTotalDifficultyByHash _).expects(blockHeader1.parentHash).returning(Some(parentDifficulty))

        toMockFunction1[BlockHeader, Unit](blockchain.save(_: BlockHeader)).expects(blockHeader1)
        toMockFunction2[ByteString, BigInt, Unit](blockchain.save(_: ByteString, _: BigInt))
          .expects(blockHeader1.hash, blockHeader1.difficulty + parentDifficulty)

        val hs = baseHandlerState.copy(syncState = baseSyncState.copy(safeDownloadTarget = blockHeader1.number))
        val (resultHandler, resultMsg) = handleBlockHeaders(peer1, Seq(blockHeader1), hs, discardLastBlocks, blacklist)
        resultHandler.syncState.receiptsQueue shouldBe blockHeader1ExpectedInQueue
        resultHandler.syncState.blockBodiesQueue shouldBe blockHeader1ExpectedInQueue
        resultMsg shouldBe UpdateTargetBlock(ImportedLastBlock)
      }

      "handle block validation error if validation failed" when {

        "header number is greater than safe download target" in {
          blockHeaderValidationFailed

          val syncState = FastSyncState(baseBlockHeader).copy(
            nextBlockToFullyValidate = blockHeader2.number,
            safeDownloadTarget = blockHeader2.number - 1
          )
          val handlerState = baseHandlerState.copy(syncState = syncState)
          val (resultHandler, resultMsg) =
            handleBlockHeaders(peer1, Seq(blockHeader2), handlerState, discardLastBlocks, blacklist)

          resultHandler shouldBe handlerState
          resultMsg shouldBe ProcessSyncing
        }

        "header number is less than safe download target and less than target block number" in {
          blockHeaderValidationFailed

          val syncState = FastSyncState(baseBlockHeader).copy(
            nextBlockToFullyValidate = blockHeader2.number,
            safeDownloadTarget = blockHeader2.number + 1,
            targetBlock = defaultTargetBlock.copy(number = blockHeader2.number + 1)
          )
          val handlerState = baseHandlerState.copy(syncState = syncState)
          val (resultHandler, resultMsg) =
            handleBlockHeaders(peer1, Seq(blockHeader2), handlerState, discardLastBlocks, blacklist)

          val expectedNextBlockToFullyValidate = blockHeader2.number - syncConfig.fastSyncBlockValidationN
          val expectedBestBlockNumber = expectedNextBlockToFullyValidate - 1

          resultHandler should not equal handlerState
          resultHandler.syncState.bestBlockHeaderNumber shouldBe expectedBestBlockNumber.max(0)
          resultHandler.syncState.nextBlockToFullyValidate shouldBe expectedNextBlockToFullyValidate.max(0)
          resultMsg shouldBe ProcessSyncing
        }

        "header number is less than safe download target but greater than target block number" in {
          blockHeaderValidationFailed

          val syncState = FastSyncState(baseBlockHeader).copy(
            nextBlockToFullyValidate = blockHeader2.number,
            safeDownloadTarget = blockHeader2.number + 1,
            targetBlock = defaultTargetBlock.copy(number = blockHeader2.number - 1)
          )
          val handlerState = baseHandlerState.copy(syncState = syncState)
          val (resultHandler, resultMsg) =
            handleBlockHeaders(peer1, Seq(blockHeader2), handlerState, discardLastBlocks, blacklist)

          val expectedNextBlockToFullyValidate = blockHeader2.number - syncConfig.fastSyncBlockValidationN
          val expectedBestBlockNumber = expectedNextBlockToFullyValidate - 1

          resultHandler should not equal handlerState
          resultHandler.syncState.bestBlockHeaderNumber shouldBe expectedBestBlockNumber.max(0)
          resultHandler.syncState.nextBlockToFullyValidate shouldBe expectedNextBlockToFullyValidate.max(0)
          resultMsg shouldBe UpdateTargetBlock(LastBlockValidationFailed)
        }
      }
    }
  }

  val blockHeaderValidator: BlockHeaderValidator = mock[BlockHeaderValidator]
  val blockHeader1ExpectedInQueue: Seq[ByteString] = Seq(hash1)

  val appStateStorage: AppStateStorage = mock[AppStateStorage]
  def discardLastBlocks(startBlock: BigInt, blocksToDiscard: Int): AppStateStorage = appStateStorage

  def blockHeaderValidationFailed: CallHandler3[String, Any, Any, Unit] = {
    blacklist expects(peer1.id, syncConfig.blacklistDuration, *) once()
    (blockHeaderValidator.validate _).expects(blockHeader2, *).returning(Left(HeaderParentNotFoundError))
    (validators.blockHeaderValidator _).expects().returning(blockHeaderValidator).once()
    (log.warning: (String, Any, Any) => Unit).expects(*, *, *)
  }

}
