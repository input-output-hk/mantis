package io.iohk.ethereum.ets.blockchain

import akka.util.ByteString
import io.circe.generic.JsonCodec
import io.circe.{Decoder, Encoder, HCursor}
import io.iohk.ethereum.domain._
import io.circe.generic.auto._
import io.iohk.ethereum.ets.vm.AccountState

case class BlockChainScenarioGroup(
  name: String,
  scenarios: Map[String, testBlockChainScenario]
)

case class testBlockChainScenario(
   blocks: List[testBlock],
   genesisBlockHeader: testBlockHeader,
   genesisRLP: Option[ByteString],
   lastblockhash: ByteString,
   network: String,
   postState: Map[Address, AccountState],
   pre: Map[Address, AccountState]
)


case class testBlock(
  rlp: String,
  blockHeader: Option[testBlockHeader],
  transactions: Option[Seq[testTransaction]],
  uncleHeaders: Option[Seq[testBlockHeader]],
  expectExceptionByzantium: Option[String],
  expectExceptionConstantinople: Option[String],
  expectExceptionEIP150: Option[String],
  expectExceptionEIP158: Option[String],
  expectExceptionFrontier: Option[String],
  expectExceptionHomestead: Option[String]
)


case class testTransaction(
  nonce: BigInt,
  gasPrice: BigInt,
  gasLimit: BigInt,
  to: Option[Address],
  value: BigInt,
  data:ByteString,
  r: BigInt,
  s: BigInt,
  v: ByteString
)

case class testBlockHeader(
  parentHash: ByteString,
  uncleHash: ByteString,
  coinbase: ByteString,
  stateRoot: ByteString,
  transactionsTrie: ByteString,
  receiptTrie: ByteString,
  bloom: ByteString,
  difficulty: BigInt,
  number: BigInt,
  gasLimit: BigInt,
  gasUsed: BigInt,
  timestamp: Long,
  extraData: ByteString,
  mixHash: ByteString,
  nonce: ByteString
)


//sealed trait GenBlock
//
//case class testBlock (
//  blockHeader: testBlockHeader,
//  rlp: ByteString,
//  transactions: Seq[testTransaction],
//  uncleHeaders: Seq[testBlockHeader]
//) extends GenBlock
//
//
//case class InfoBlock(
//  expectExceptionByzantium: String,
//  expectExceptionConstantinople: String,
//  expectExceptionEIP150: String,
//  expectExceptionEIP158: String,
//  expectExceptionFrontier: String,
//  expectExceptionHomestead: String,
//  rlp: ByteString
//)  extends GenBlock
//
//object GenBlock extends Decoders {
//  implicit val decodeGenBlock: Decoder[GenBlock] = io.circe.generic.semiauto.deriveDecoder[GenBlock]
//}

//case class Block(
//                  blockHeader: BlockHeader,
//                  rlp: ByteString,
//                  transactions: Seq[SignedTransaction],
//                  uncleHeaders: Seq[BlockHeader]
//                )
//
//
//case class BlockChainScenario(
//  blocks: List[Block],
//  genesisBlockHeader: List[BlockHeader],
//  genesisRLP: ByteString,
//  lastblockhash: ByteString,
//  network: String,
//  postState: Option[Map[Address, Account]],
//  pre: Map[Address, Account]
//)
//object InfoBlock extends Decoders {
//  implicit val decodetestBlock: Decoder[InfoBlock] = new Decoder[InfoBlock] {
//    final def apply(c: HCursor): Decoder.Result[InfoBlock] = {
//      for {
//        ommersHash <- c.downField("expectExceptionByzantium").as[String]
//        beneficiary <- c.downField("expectExceptionConstantinople").as[String]
//        stateRoot <- c.downField("expectExceptionEIP150").as[String]
//        transactionsTrie <- c.downField("expectExceptionEIP158").as[String]
//        receiptTrie <- c.downField("expectExceptionFrontier").as[String]
//        bloom <- c.downField("expectExceptionHomestead").as[String]
//        rlp <- c.downField("rlp").as[ByteString]
//      } yield {
//        InfoBlock(ommersHash,beneficiary,stateRoot,transactionsTrie,receiptTrie,bloom,rlp)
//      }
//    }
//  }
//}

/*case class Transaction(
                        nonce: BigInt,
                        gasPrice: BigInt,
                        gasLimit: BigInt,
                        receivingAddress: Option[Address],
                        value: BigInt,
                        payload: ByteString) */




