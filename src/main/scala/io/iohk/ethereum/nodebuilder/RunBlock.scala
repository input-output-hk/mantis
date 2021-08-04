package io.iohk.ethereum.nodebuilder
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.client.RequestBuilding.Post
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.util.ByteString
import io.iohk.ethereum.consensus.ConsensusImpl
import io.iohk.ethereum.consensus.mining.{Mining, StdMiningBuilder}
import io.iohk.ethereum.consensus.pow.validators.EthashBlockHeaderValidator
import io.iohk.ethereum.domain.{Address, Block, BlockBody, BlockHeader, LegacyTransaction, SignedTransaction}
import io.iohk.ethereum.utils.NumericUtils
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object RunBlock {
  def main(args: Array[String]): Unit = {
    val node = new RunBlockNode()
    node.run()
  }
}

class RunBlockNode extends BaseNode with StdMiningBuilder {
  def run(): Unit = {
    loadGenesisData()

    runDBConsistencyCheck()

    val block = Fetcher.get(46383)
    val res = blockExecution.executeAndValidateBlock(block, true)
    println(res)
  }
}

object Fetcher {
  def get(bn: Int): Block = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import org.json4s._
    import org.json4s.native.JsonMethods._
    implicit val system = ActorSystem("SingleRequest")

    implicit val formats: Formats = DefaultFormats

    //11455798

    def run(n: Int): Block = {
      val r: HttpRequest = Post(
        "https://www.ethercluster.com/etc",
        s"""{"method":"eth_getBlockByNumber","params":["$n", true],"id":1,"jsonrpc":"2.0"}"""
      )
      val resp = Http().singleRequest(r)

      val meh = Await.result(resp, Duration.Inf)

      val res = Unmarshal(meh).to[String]

      val dataRaw = Await.result(res, Duration.Inf)

      val result = parse(dataRaw) \ "result"

      println(result)

      val bh = result.extract[Data]
      val bb = result.extract[Body]

      println(bb)

      Block(bh.to, bb.to)
    }
    run(bn)
  }

  case class ST(
      from: String,
      gas: String,
      gasPrice: String,
      hash: String,
      input: String,
      nonce: String,
      to: String,
      value: String,
      v: String,
      r: String,
      s: String
  )
  //

  case class Body(
      uncles: List[String],
      transactions: List[ST]
  ) {
    def to: BlockBody =
      BlockBody(
        transactions
          .map(st =>
            SignedTransaction(
              LegacyTransaction(
                NumericUtils.parseHexOrDecNumber(st.nonce),
                NumericUtils.parseHexOrDecNumber(st.gasPrice),
                NumericUtils.parseHexOrDecNumber(st.gas),
                Address(st.to),
                NumericUtils.parseHexOrDecNumber(st.value),
                ByteString(st.input)
              ),
              NumericUtils.parseHexOrDecNumber(st.v).toByte,
              ByteString(st.r),
              ByteString(st.s)
            )
          ),
        Nil
      )
  }

  case class Data(
      number: String,
      hash: String,
      mixHash: String,
      parentHash: String,
      nonce: String,
      sha3Uncles: String,
      logsBloom: String,
      transactionsRoot: String,
      stateRoot: String,
      receiptsRoot: String,
      miner: String,
      difficulty: String,
      totalDifficulty: String,
      extraData: String,
      size: String,
      gasLimit: String,
      gasUsed: String,
      timestamp: String
  ) {
    def to: BlockHeader =
      BlockHeader(
        parentHash = ByteString(Hex.decode(parentHash.drop(2))),
        ommersHash = ByteString(Hex.decode(sha3Uncles.drop(2))),
        beneficiary = ByteString(Hex.decode(miner.drop(2))),
        stateRoot = ByteString(Hex.decode(stateRoot.drop(2))),
        transactionsRoot = ByteString(Hex.decode(transactionsRoot.drop(2))),
        receiptsRoot = ByteString(Hex.decode(receiptsRoot.drop(2))),
        logsBloom = ByteString(Hex.decode(logsBloom.drop(2))),
        difficulty = BigInt(difficulty.drop(2), 16),
        number = BigInt(number.drop(2), 16),
        gasLimit = BigInt(gasLimit.drop(2), 16),
        gasUsed = BigInt(gasUsed.drop(2), 16),
        unixTimestamp = BigInt(timestamp.drop(2), 16).toLong,
        extraData = ByteString(Hex.decode(extraData.drop(2))),
        mixHash = ByteString(Hex.decode(mixHash.drop(2))),
        nonce = ByteString(Hex.decode(nonce.drop(2)))
      )
  }
}
