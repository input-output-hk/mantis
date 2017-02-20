package io.iohk.ethereum.rpc

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV63.Receipt
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

    override def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] = ???

    override def getBlockBodyByHash(hash: ByteString): Option[BlockBody] = ???

    override def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = ???

    override def getEvmCodeByHash(hash: ByteString): Option[ByteString] = ???

    override def save(block: Block): Unit = ???

    override def save(blockHash: ByteString, receipts: Seq[Receipt]): Unit = ???

    override def save(hash: ByteString, evmCode: ByteString): Unit = ???

    override protected def getHashByBlockNumber(number: BigInt): Option[ByteString] = ???
  }

  val route: Route = BlockController.route(stubbedBlockchain)
}
