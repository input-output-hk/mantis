package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.{Message, MessageSerializableImplicit}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import org.bouncycastle.util.encoders.Hex

/**
  * This is PV164 version, the real one will be implemented by ETCM-355
  * A new version number will need to be proposed when checkpointing is reintroduced
  */
//FIXME This PV is considered WIP and needs to be reassessed
object PV164 {
  object Status {
    implicit class StatusEnc(val underlyingMsg: Status)
        extends MessageSerializableImplicit[Status](underlyingMsg)
        with RLPSerializable {
      override def code: Int = Codes.StatusCode

      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        RLPList(
          protocolVersion,
          networkId,
          chainWeight.totalDifficulty,
          chainWeight.lastCheckpointNumber,
          bestHash,
          genesisHash
        )
      }
    }

    implicit class StatusDec(val bytes: Array[Byte]) extends AnyVal {
      def toStatus: Status = rawDecode(bytes) match {
        case RLPList(
              protocolVersion,
              networkId,
              totalDifficulty,
              lastCheckpointNumber,
              bestHash,
              genesisHash
            ) =>
          Status(
            protocolVersion,
            networkId,
            ChainWeight(lastCheckpointNumber, totalDifficulty),
            bestHash,
            genesisHash
          )

        case _ => throw new RuntimeException("Cannot decode Status PV164 version")
      }
    }

  }

  //FIXME This PV is considered WIP and needs to be reassessed
  case class Status(
      protocolVersion: Int,
      networkId: Int,
      chainWeight: ChainWeight,
      bestHash: ByteString,
      genesisHash: ByteString
  ) extends Message {

    override def toString: String =
      s"Status { " +
        s"protocolVersion: $protocolVersion, " +
        s"networkId: $networkId, " +
        s"chainWeight: $chainWeight, " +
        s"bestHash: ${Hex.toHexString(bestHash.toArray[Byte])}, " +
        s"genesisHash: ${Hex.toHexString(genesisHash.toArray[Byte])}," +
        s"}"

    override def toShortString: String = toString

    override def code: Int = Codes.StatusCode
  }

  //FIXME This PV is considered WIP and needs to be reassessed
  object NewBlock {
    implicit class NewBlockEnc(val underlyingMsg: NewBlock)
        extends MessageSerializableImplicit[NewBlock](underlyingMsg)
        with RLPSerializable {
      import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

      override def code: Int = Codes.NewBlockCode

      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        RLPList(
          RLPList(
            block.header.toRLPEncodable,
            RLPList(block.body.transactionList.map(_.toRLPEncodable): _*),
            RLPList(block.body.uncleNodesList.map(_.toRLPEncodable): _*)
          ),
          chainWeight.totalDifficulty,
          chainWeight.lastCheckpointNumber
        )
      }
    }

    implicit class NewBlockDec(val bytes: Array[Byte]) extends AnyVal {
      import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

      def toNewBlock: NewBlock = rawDecode(bytes) match {
        case RLPList(
              RLPList(blockHeader, transactionList: RLPList, (uncleNodesList: RLPList)),
              totalDifficulty,
              lastCheckpointNumber
            ) =>
          NewBlock(
            Block(
              blockHeader.toBlockHeader,
              BlockBody(transactionList.items.map(_.toSignedTransaction), uncleNodesList.items.map(_.toBlockHeader))
            ),
            ChainWeight(lastCheckpointNumber, totalDifficulty)
          )
        case _ => throw new RuntimeException("Cannot decode NewBlock PV164 version")
      }
    }
  }

  //FIXME This PV is considered WIP and needs to be reassessed
  case class NewBlock(block: Block, chainWeight: ChainWeight) extends Message {
    override def toString: String =
      s"NewBlock { " +
        s"block: $block, " +
        s"chainWeight: $chainWeight" +
        s"}"

    override def toShortString: String =
      s"NewBlock { " +
        s"block.header: ${block.header}, " +
        s"chainWeight: $chainWeight" +
        s"}"

    override def code: Int = Codes.NewBlockCode
  }
}
