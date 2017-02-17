package io.iohk.ethereum.rpc

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.blockchain.Blockchain
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.rpc.BlockController.BlockView
import org.scalatest.{FlatSpec, Matchers}

class BlockControllerSpec extends FlatSpec with Matchers with ScalatestRouteTest with BlockRouteSetup {

  "BlockController" should "return the block when queried by block number" in {
    val blockHeader = Fixtures.Blocks.ValidBlock.header
    generateRequest("getBlockByNumber", blockHeader.number) ~> route ~> check {
      status.isSuccess()
      assert(responseAs[BlockView] equals BlockView(
        difficulty = blockHeader.difficulty,
        extraData = blockHeader.extraData,
        gasLimit = blockHeader.gasLimit,
        gasUsed = blockHeader.gasUsed,
        hash = blockHeader.hash,
        logsBloom = blockHeader.logsBloom,
        beneficiary = blockHeader.beneficiary,
        mixHash = blockHeader.mixHash,
        nonce = blockHeader.nonce,
        number = blockHeader.number,
        parentHash = blockHeader.parentHash,
        receiptsRoot = blockHeader.receiptsRoot,
        ommersHash = blockHeader.ommersHash,
        stateRoot = blockHeader.stateRoot,
        unixTimestamp = blockHeader.unixTimestamp,
        transactions = Fixtures.Blocks.ValidBlock.transactionHashes,
        transactionsRoot = blockHeader.transactionsRoot,
        uncles = Seq(),
        size = Fixtures.Blocks.ValidBlock.size
      ))
    }
  }

  "BlockController" should "return an error when queried by block number that doesn't exist" in {
    generateRequest("getBlockByNumber", Fixtures.Blocks.ValidBlock.header.number + 1) ~> route ~> check {
      status == StatusCodes.NotFound
    }
  }
}

trait BlockRouteSetup {
  def generateRequest(url: String, blockId: BigInt): HttpRequest = HttpRequest(HttpMethods.GET, uri = s"/$url/$blockId")

  private val stubbedBlockchain: Blockchain = new Blockchain {

    override def getBlockByNumber(number: BigInt): Option[Block] =
      if (number == Fixtures.Blocks.ValidBlock.header.number) Some(Fixtures.Blocks.ValidBlock.block)
      else None

    override protected def receiptStorage: ReceiptStorage = ???

    override protected def evmCodeStorage: EvmCodeStorage = ???

    override protected def blockHeadersStorage: BlockHeadersStorage = ???

    override protected def blockBodiesStorage: BlockBodiesStorage = ???

    override protected def blockNumberMappingStorage: BlockNumberMappingStorage = ???
  }

  val route: Route = BlockController.route(stubbedBlockchain)
}
