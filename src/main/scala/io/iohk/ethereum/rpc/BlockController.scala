package io.iohk.ethereum.rpc

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Block, Blockchain, SignedTransaction}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.rpc.customserializers.ByteStringJsonSerializer._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object BlockController {

  final case class BlockView(
                              difficulty: BigInt,
                              extraData: ByteString,
                              gasLimit: BigInt,
                              gasUsed: BigInt,
                              hash: ByteString,
                              logsBloom: ByteString,
                              beneficiary: ByteString,
                              mixHash: ByteString,
                              nonce: ByteString,
                              number: BigInt,
                              parentHash: ByteString,
                              receiptsRoot: ByteString,
                              ommersHash: ByteString,
                              stateRoot: ByteString,
                              unixTimestamp: Long,
                              transactions: Seq[ByteString],
                              transactionsRoot: ByteString,
                              uncles: Seq[ByteString],
                              size: Long
                            )

  implicit val itemFormat = jsonFormat19(BlockView)

  // FIXME This route is temporal until we switch to JSON-RPC
  def route(blockchain: Blockchain): Route = get {
    path("getBlockByNumber" / """\d+""".r ) { blockNumber =>
      val maybeBlockView: Future[Option[BlockView]] = getBlockByNumber(BigInt(blockNumber), blockchain)
      onSuccess(maybeBlockView) {
        case Some(item) => complete(item)
        case None => complete(StatusCodes.NotFound)
      }
    }
  }

  private def getBlockByNumber(itemId: BigInt, blockchain: Blockchain): Future[Option[BlockView]] = Future {
    blockchain.getBlockByNumber(itemId).map(blockToView)
  }

  private def blockToView(block: Block): BlockView = {
    import block.body._
    import block.header._
    BlockView(
      difficulty = difficulty,
      extraData = extraData,
      gasLimit = gasLimit,
      gasUsed = gasUsed,
      hash = hash,
      logsBloom = logsBloom,
      beneficiary = beneficiary,
      mixHash = mixHash,
      nonce = nonce,
      number = number,
      parentHash = parentHash,
      receiptsRoot = receiptsRoot,
      ommersHash = ommersHash,
      stateRoot = stateRoot,
      unixTimestamp = unixTimestamp,
      transactions = transactionList.map(tx => ByteString(kec256(encode[SignedTransaction](tx)))),
      transactionsRoot = transactionsRoot,
      uncles = uncleNodesList.map(h => h.hash),
      size = Block.size(block)
    )
  }

}
