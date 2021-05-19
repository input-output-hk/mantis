package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.network.p2p.messages.CommonMessages._
import io.iohk.ethereum.network.p2p.messages.PV61.BlockHashesFromNumber
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.p2p.{EthereumMessageDecoder, NetworkMessageDecoder}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MessagesSerializationSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  // TODO: add tests for messages from PV63
  "Wire Protocol" when {

    "encoding and decoding Hello" should {
      "return same result" in {
        verify(
          Hello(1, "teest", Seq(Capability("Sample", 1), Capability("Sample", 2)), 1, ByteString("Id")),
          (m: Hello) => m.toBytes,
          Hello.code,
          ProtocolVersions.PV63
        )
      }
    }

    "encoding and decoding Disconnect" should {
      "return same result" in {
        verify(
          Disconnect(Disconnect.Reasons.AlreadyConnected),
          (m: Disconnect) => m.toBytes,
          Disconnect.code,
          ProtocolVersions.PV63
        )
      }
    }

    "encoding and decoding Ping" should {
      "return same result" in {
        verify(Ping(), (m: Ping) => m.toBytes, Ping.code, ProtocolVersions.PV63)
      }
    }

    "encoding and decoding Pong" should {
      "return same result" in {
        verify(Pong(), (m: Pong) => m.toBytes, Pong.code, ProtocolVersions.PV63)
      }
    }
  }

  "Common Messages" when {
    "encoding and decoding Status" should {
      "return same result for Status v63" in {
        val msg = Status(1, 2, 2, ByteString("HASH"), ByteString("HASH2"))
        verify(msg, (m: Status) => m.toBytes, Codes.StatusCode, ProtocolVersions.PV63)
      }
    }

    "encoding and decoding SignedTransactions" should {
      "return same result" in {
        val msg = SignedTransactions(Fixtures.Blocks.Block3125369.body.transactionList)
        verify(msg, (m: SignedTransactions) => m.toBytes, Codes.SignedTransactionsCode, ProtocolVersions.PV63)
      }
    }

    "encoding and decoding NewBlock" should {
      "return same result for NewBlock v63" in {
        val msg = NewBlock(Fixtures.Blocks.Block3125369.block, 2323)
        verify(msg, (m: NewBlock) => m.toBytes, Codes.NewBlockCode, ProtocolVersions.PV63)
      }
    }
  }

  "PV164" when {
    "encoding and decoding Status" should {
      "return same result for Status v164" in {
        val msg = PV164.Status(1, 2, ChainWeight(2, 5), ByteString("HASH"), ByteString("HASH2"))
        verify(msg, (m: PV164.Status) => m.toBytes, Codes.StatusCode, ProtocolVersions.PV164)
      }
    }

    "encoding and decoding NewBlock" should {
      "return same result for NewBlock v64" in {
        val msg = PV164.NewBlock(Fixtures.Blocks.Block3125369.block, ChainWeight(2323, 21))
        verify(msg, (m: PV164.NewBlock) => m.toBytes, Codes.NewBlockCode, ProtocolVersions.PV164)
      }
    }
  }

  "PV61" when {
    val version = ProtocolVersions.PV61
    "encoding and decoding NewBlockHashes" should {
      "return same result" in {
        val msg = PV61.NewBlockHashes(Seq(ByteString("23"), ByteString("10"), ByteString("36")))
        verify(msg, (m: PV61.NewBlockHashes) => m.toBytes, Codes.NewBlockHashesCode, version)
      }
    }

    "encoding and decoding BlockHashesFromNumber" should {
      "return same result" in {
        val msg = BlockHashesFromNumber(1, 2)
        verify(msg, (m: BlockHashesFromNumber) => m.toBytes, Codes.BlockHashesFromNumberCode, version)
      }
    }
  }

  "PV62" when {
    val version = ProtocolVersions.PV62
    "encoding and decoding NewBlockHashes" should {
      "return same result" in {
        val msg = PV62.NewBlockHashes(Seq(BlockHash(ByteString("hash1"), 1), BlockHash(ByteString("hash2"), 2)))
        verify(msg, (m: PV62.NewBlockHashes) => m.toBytes, Codes.NewBlockHashesCode, version)
      }
    }

    "encoding and decoding BlockBodies" should {
      "return same result" in {
        val msg = BlockBodies(Seq(Fixtures.Blocks.Block3125369.body, Fixtures.Blocks.DaoForkBlock.body))
        verify(msg, (m: BlockBodies) => m.toBytes, Codes.BlockBodiesCode, version)
      }
    }

    "encoding and decoding GetBlockBodies" should {
      "return same result" in {
        val msg = GetBlockBodies(Seq(ByteString("111"), ByteString("2222")))
        verify(msg, (m: GetBlockBodies) => m.toBytes, Codes.GetBlockBodiesCode, version)
      }
    }

    "encoding and decoding BlockHeaders" should {
      "return same result" in {
        val msg = BlockHeaders(Seq(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.DaoForkBlock.header))
        verify(msg, (m: BlockHeaders) => m.toBytes, Codes.BlockHeadersCode, version)
      }
    }

    "encoding and decoding GetBlockHeaders" should {
      "return same result" in {
        verify(
          GetBlockHeaders(Left(1), 1, 1, false),
          (m: GetBlockHeaders) => m.toBytes,
          Codes.GetBlockHeadersCode,
          version
        )
        verify(
          GetBlockHeaders(Right(ByteString("1" * 32)), 1, 1, true),
          (m: GetBlockHeaders) => m.toBytes,
          Codes.GetBlockHeadersCode,
          version
        )
      }
    }
  }

  val messageDecoder = NetworkMessageDecoder orElse EthereumMessageDecoder

  def verify[T](msg: T, encode: T => Array[Byte], code: Int, version: Int): Unit =
    messageDecoder.fromBytes(code, encode(msg), version) shouldEqual msg

}
