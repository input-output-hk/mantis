package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.rlp.RLPImplicits._
import org.spongycastle.util.encoders.Hex


object CommonMessages {
  object Status {
    implicit val rlpEncDec = new RLPEncoder[Status] with RLPDecoder[Status] {
      override def encode(obj: Status): RLPEncodeable = {
        import obj._
        RLPList(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash)
      }

      override def decode(rlp: RLPEncodeable): Status = rlp match {
        case RLPList(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash) =>
          Status(protocolVersion, networkId, totalDifficulty, bestHash, genesisHash)
        case _ => throw new RuntimeException("Cannot decode Status")
      }
    }

    val code: Int = Message.SubProtocolOffset + 0x00
  }

  case class Status(protocolVersion: Int, networkId: Int, totalDifficulty: BigInt, bestHash: ByteString, genesisHash: ByteString) extends Message {
    override def code: Int = Status.code

    override def toString: String = {
      s"""Status {
         |protocolVersion: $protocolVersion
         |networkId: $networkId
         |totalDifficulty: $totalDifficulty
         |bestHash: ${Hex.toHexString(bestHash.toArray[Byte])}
         |genesisHash: ${Hex.toHexString(genesisHash.toArray[Byte])}
         |}""".stripMargin
    }
  }

  object SignedTransactions {

    implicit val txRlpEncDec = new RLPEncoder[SignedTransaction] with RLPDecoder[SignedTransaction] {

      override def encode(signedTx: SignedTransaction): RLPEncodeable = {
        import signedTx._
        import signedTx.tx._
        RLPList(nonce, gasPrice, gasLimit, receivingAddress.toArray, value,
          payload, pointSign, signatureRandom, signature)
      }

      override def decode(rlp: RLPEncodeable): SignedTransaction = rlp match {
        case RLPList(nonce, gasPrice, gasLimit, (receivingAddress: RLPValue), value,
        payload, pointSign, signatureRandom, signature) =>
          SignedTransaction(
            Transaction(nonce, gasPrice, gasLimit, Address(receivingAddress.bytes), value, payload), pointSign, signatureRandom, signature)
      }

    }

    implicit val txsRlpEncDec = new RLPEncoder[SignedTransactions] with RLPDecoder[SignedTransactions] {

      override def encode(obj: SignedTransactions): RLPEncodeable = {
        import obj._
        toEncodeableList(txs)
      }

      override def decode(rlp: RLPEncodeable): SignedTransactions = rlp match {
        case rlpList: RLPList => SignedTransactions(fromEncodeableList[SignedTransaction](rlpList))
        case _ => throw new RuntimeException("Cannot decode SignedTransactions")
      }

    }

    val code: Int = Message.SubProtocolOffset + 0x02
  }

  case class SignedTransactions(txs: Seq[SignedTransaction]) extends Message {
    override def code: Int = SignedTransactions.code
  }

  object NewBlock {

    import SignedTransactions.txRlpEncDec

    implicit val rlpEncDec = new RLPEncoder[NewBlock] with RLPDecoder[NewBlock] {

      override def encode(obj: NewBlock): RLPEncodeable = {
        import obj._
        RLPList(
          RLPList(
            block.header,
            toEncodeableList(block.body.transactionList),
            toEncodeableList(block.body.uncleNodesList)
          ),
          totalDifficulty
        )
      }

      override def decode(rlp: RLPEncodeable): NewBlock = rlp match {
        case RLPList(RLPList(blockHeader, (transactionList: RLPList), (uncleNodesList: RLPList)), totalDifficulty) =>
          NewBlock(
            Block(
              headerRlpEncDec.decode(blockHeader),
              BlockBody(
                fromEncodeableList[SignedTransaction](transactionList),
                fromEncodeableList[BlockHeader](uncleNodesList))),
            totalDifficulty
          )
        case _ => throw new RuntimeException("Cannot decode NewBlock")
      }

    }

    val code: Int = Message.SubProtocolOffset + 0x07
  }

  case class NewBlock(block: Block, totalDifficulty: BigInt) extends Message {
    override def code: Int = NewBlock.code

    override def toString: String = {
      s"""NewBlock {
         |block: $block
         |totalDifficulty: $totalDifficulty
         |}""".stripMargin
    }
  }
}
