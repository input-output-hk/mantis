package io.iohk.ethereum.ets.blockchain

import io.circe.parser.decode
import io.iohk.ethereum.ets.vm.Scenario
import akka.util.ByteString
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.iohk.ethereum.domain.{Address, BlockHeader}
import io.iohk.ethereum.utils.NumericUtils.parseHexOrDecNumber
import io.iohk.ethereum.vm.UInt256
import org.spongycastle.util.encoders.Hex

import scala.util.Try


object BlockChainScenarioParser extends Decoders {

  //  implicit val decodeFoo: Decoder[BlockHeader] = new Decoder[BlockHeader] {
  //    final def apply(c: HCursor): Decoder.Result[BlockHeader] =
  //      for {
  //        parentHash <- c.downField("parentHash").as[ByteString]
  //        ommersHash <- c.downField("uncleHash").as[ByteString]
  //        beneficiary <- c.downField("coinbase").as[ByteString]
  //        stateRoot <- c.downField("stateRoot").as[ByteString]
  //        transactionsTrie <- c.downField("transactionsTrie").as[ByteString]
  //        receiptTrie <- c.downField("receiptTrie").as[ByteString]
  //        bloom <- c.downField("bloom").as[ByteString]
  //        difficulty <- c.downField("difficulty").as[BigInt]
  //        number <- c.downField("number").as[BigInt]
  //        gasLimit <- c.downField("gasLimit").as[BigInt]
  //        gasUsed <- c.downField("gasUsed").as[BigInt]
  //        timestamp <- c.downField("timestamp").as[Long]
  //        extraData <- c.downField("extraData").as[ByteString]
  //        mixHash <- c.downField("mixHash").as[ByteString]
  //        nonce <- c.downField("nonce").as[ByteString]
  //      } yield {
  //        BlockHeader(
  //          parentHash,
  //          ommersHash,
  //          beneficiary,
  //          stateRoot,
  //          transactionsTrie,
  //          receiptTrie,
  //          bloom,
  //          difficulty,
  //          number,
  //          gasLimit,
  //          gasUsed,
  //          timestamp,
  //          extraData,
  //          mixHash,
  //          nonce
  //        )
  //      }
  //  }


  def parse(json: String): Map[String, testBlockChainScenario] =
    decode[Map[String, testBlockChainScenario]](json).fold(throw _, identity)
}
