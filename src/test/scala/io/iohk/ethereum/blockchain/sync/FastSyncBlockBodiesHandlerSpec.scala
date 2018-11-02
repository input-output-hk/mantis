package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.consensus.validators.std.StdBlockValidator.{ BlockReceiptsHashError, BlockValid }
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

// scalastyle:off magic.number
class FastSyncBlockBodiesHandlerSpec extends FastSyncHandlersSetup with FastSyncBlockBodiesHandler {

  "FastSyncBlockBodiesHandler" should {
    "blacklist peer" when {
      "got empty block bodies response from known hashes" in {
        blacklist expects(peer1.id, syncConfig.blacklistDuration, *) once()
        baseHandlerState.syncState.blockBodiesQueue shouldBe Nil

        val result: FastSyncHandlerState =
          handleBlockBodies(peer1, requestedHashes, Seq.empty, baseHandlerState, blacklist, updateBestBlock)
        result.syncState.blockBodiesQueue shouldBe requestedHashes
      }

      "got block bodies not matching block headers" in {
        (blockchain.getBlockHeaderByHash _).expects(hash1).returning(Some(blockHeader1))
        (blockchain.getBlockHeaderByHash _).expects(hash2).returning(Some(blockHeader2))
        (blockValidator.validateHeaderAndBody _).expects(blockHeader1, emptyBlockBody).returning(Right(BlockValid))
        (blockValidator.validateHeaderAndBody _).expects(blockHeader2, emptyBlockBody).returning(Left(BlockReceiptsHashError))
        (validators.blockValidator _).expects().returning(blockValidator).twice()
        blacklist expects(peer1.id, syncConfig.blacklistDuration, *) once()

        baseHandlerState.syncState.blockBodiesQueue shouldBe Nil

        val result: FastSyncHandlerState =
          handleBlockBodies(peer1, requestedHashes, bodies, baseHandlerState, blacklist, updateBestBlock)
        result.syncState.blockBodiesQueue shouldBe requestedHashes
      }
    }

    "restart download process if got DbError from blocks validation" in {
      (blockchain.getBlockHeaderByHash _).expects(hash1).returning(Some(blockHeader1))
      (blockchain.getBlockHeaderByHash _).expects(hash2).returning(None)
      (blockValidator.validateHeaderAndBody _).expects(blockHeader1, emptyBlockBody).returning(Right(BlockValid))
      (validators.blockValidator _).expects().returning(blockValidator).once()
      (log.debug: String => Unit).expects(*).returning(())

      val expectedBestBlock: BigInt =
        (baseHandlerState.syncState.bestBlockHeaderNumber - 2 * syncConfig.blockHeadersPerRequest).max(0)

      val result: FastSyncState =
        handleBlockBodies(peer1, requestedHashes, bodies, baseHandlerState, blacklist, updateBestBlock).syncState
      result.blockBodiesQueue shouldBe Nil
      result.receiptsQueue shouldBe Nil
      result.bestBlockHeaderNumber shouldBe expectedBestBlock
    }

    "insert blocks if validation passes" in {
      (blockchain.getBlockHeaderByHash _).expects(hash1).returning(Some(blockHeader1))
      (blockchain.getBlockHeaderByHash _).expects(hash2).returning(Some(blockHeader2))
      (blockValidator.validateHeaderAndBody _).expects(blockHeader1, emptyBlockBody).returning(Right(BlockValid))
      (blockValidator.validateHeaderAndBody _).expects(blockHeader2, emptyBlockBody).returning(Right(BlockValid))
      (validators.blockValidator _).expects().returning(blockValidator).twice()
      updateBestBlock expects * once()

      // ### Compiler bug? ###
      // For some reason implicit resolution is not working well when we reference method
      // using 'methodName(_: Type1, _: Type2)' notation instead of 'methodName _',
      // which is required in this case since there are many overloaded 'save' methods in Blockchain trait.
      //
      // When '(blockchain.save(_: ByteString, _: BlockBody)).expects(...)' is called
      // 'org.scalamock.clazz.Mock.toMockFunction1' implicit conversion is incorrectly picked up instead of
      // 'org.scalamock.clazz.Mock.toMockFunction2' which leads to runtime exception during test
      // (in 'org.scalatest.OutcomeOf.outcomeOf') where MockFunction1 is casted to MockFunction2.
      toMockFunction2[ByteString, BlockBody, Unit](blockchain.save(_: ByteString, _: BlockBody))
        .expects(*, emptyBlockBody).twice()

      val result: FastSyncHandlerState =
        handleBlockBodies(peer1, requestedHashes, bodies, baseHandlerState, blacklist, updateBestBlock)
      result shouldBe baseHandlerState
    }

  }

  val blockValidator: BlockValidator = mock[BlockValidator]

}
