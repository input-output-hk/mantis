package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.{ BlockReceiptsHashError, BlockValid }
import io.iohk.ethereum.domain.Receipt

class FastSyncReceiptsHandlerSpec extends FastSyncHandlersSetup with FastSyncReceiptsHandler with ObjectGenerators {

  "FastSyncReceiptsHandler" should {
    "blacklist peer" when {
      "got empty receipts for known hashes" in {
        blacklist expects(peer1.id, syncConfig.blacklistDuration, *) once()
        updateBestBlock expects * once()

        val resultHandler = handleReceipts(peer1, requestedHashes, Seq.empty, baseHandlerState, blacklist, updateBestBlock)
        resultHandler.syncState.receiptsQueue shouldBe requestedHashes
      }

      "got invalid receipts for known hashes" in {
        (blockchain.getBlockHeaderByHash _).expects(hash1).returning(Some(blockHeader1))
        (blockValidator.validateBlockAndReceipts _)
          .expects(blockHeader1, *)
          .returning(Left(BlockReceiptsHashError))
          .twice()
        (validators.blockValidator _).expects().returning(blockValidator).twice()
        blacklist expects(peer1.id, syncConfig.blacklistDuration, *) once()

        handleReceipts(peer1, Seq(hash1), receipts, baseHandlerState, blacklist, updateBestBlock)
      }
    }

    "change handler state" when {
      "block header is missing for known hashes" in {
        (blockchain.getBlockHeaderByHash _).expects(hash1).returning(None)
        (log.debug: String => Unit).expects(*).once()

        val expectedBestBlock = (baseSyncState.bestBlockHeaderNumber - 2 * syncConfig.blockHeadersPerRequest).max(0)
        val resultHandler = handleReceipts(peer1, Seq(hash1), receipts, baseHandlerState, blacklist, updateBestBlock)
        resultHandler.syncState.bestBlockHeaderNumber shouldBe expectedBestBlock
      }

      "still have block headers with remaining receipts" in {
        (blockchain.getBlockHeaderByHash _).expects(hash1).returning(Some(blockHeader1))
        (blockValidator.validateBlockAndReceipts _).expects(blockHeader1, *).returning(Right(BlockValid))
        (validators.blockValidator _).expects().returning(blockValidator).once()
        toMockFunction2[ByteString, Seq[Receipt], Unit](blockchain.save(_: ByteString, _: Seq[Receipt]))
          .expects(hash1, receipts1)
        updateBestBlock expects Seq(hash1) once()

        val resultHandler = handleReceipts(peer1, requestedHashes, receipts, baseHandlerState, blacklist, updateBestBlock)
        resultHandler.syncState.receiptsQueue shouldBe Seq(hash2)
      }
    }

    "not change handler state if all receipt were handled correctly" in {
      (blockchain.getBlockHeaderByHash _).expects(hash1).returning(Some(blockHeader1))
      (blockchain.getBlockHeaderByHash _).expects(hash2).returning(Some(blockHeader2))
      (blockValidator.validateBlockAndReceipts _).expects(blockHeader1, receipts1).returning(Right(BlockValid))
      (blockValidator.validateBlockAndReceipts _).expects(blockHeader2, receipts2).returning(Right(BlockValid))
      (validators.blockValidator _).expects().returning(blockValidator).twice()
      toMockFunction2[ByteString, Seq[Receipt], Unit](blockchain.save(_: ByteString, _: Seq[Receipt]))
        .expects(hash1, receipts1)
      toMockFunction2[ByteString, Seq[Receipt], Unit](blockchain.save(_: ByteString, _: Seq[Receipt]))
        .expects(hash2, receipts2)
      updateBestBlock expects requestedHashes once()

      val resultHandler =
        handleReceipts(peer1, requestedHashes, Seq(receipts1, receipts2), baseHandlerState, blacklist, updateBestBlock)
      resultHandler shouldBe baseHandlerState
    }
  }

  val blockValidator: BlockValidator = mock[BlockValidator]

  val receipts1 = Seq(receiptGen().sample.get)
  val receipts2 = Seq(receiptGen().sample.get)
  val receipts: Seq[Seq[Receipt]] = Seq(receipts1)

}
