package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.forkid.ForkId
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.p2p.NetworkMessageDecoder
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages._
import io.iohk.ethereum.network.p2p.messages.ETH61.BlockHashesFromNumber
import io.iohk.ethereum.network.p2p.messages.ETH62._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._

class MessagesSerializationSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  // TODO: add tests for messages from ETH63
  "Wire Protocol" when {

    "encoding and decoding Hello" should {
      "return same result" in {
        verify(
          Hello(1, "teest", Seq(Capability("Sample", 1), Capability("Sample", 2)), 1, ByteString("Id")),
          (m: Hello) => m.toBytes,
          Hello.code,
          ProtocolVersions.ETH63
        )
      }
    }

    "encoding and decoding Disconnect" should {
      "return same result" in {
        verify(
          Disconnect(Disconnect.Reasons.AlreadyConnected),
          (m: Disconnect) => m.toBytes,
          Disconnect.code,
          ProtocolVersions.ETH63
        )
      }
    }

    "encoding and decoding Ping" should {
      "return same result" in {
        verify(Ping(), (m: Ping) => m.toBytes, Ping.code, ProtocolVersions.ETH63)
      }
    }

    "encoding and decoding Pong" should {
      "return same result" in {
        verify(Pong(), (m: Pong) => m.toBytes, Pong.code, ProtocolVersions.ETH63)
      }
    }
  }

  "Common Messages" when {
    "encoding and decoding SignedTransactions" should {
      "return same result" in {
        val msg = SignedTransactions(Fixtures.Blocks.Block3125369.body.transactionList)
        verify(msg, (m: SignedTransactions) => m.toBytes, Codes.SignedTransactionsCode, ProtocolVersions.ETH63)
      }
    }

    "encoding and decoding NewBlock" should {
      "return same result for NewBlock v63" in {
        val msg = NewBlock(Fixtures.Blocks.Block3125369.block, 2323)
        verify(msg, (m: NewBlock) => m.toBytes, Codes.NewBlockCode, ProtocolVersions.ETH63)
      }
    }
  }

  "ETC64" when {
    "encoding and decoding Status" should {
      "return same result for Status v64" in {
        val msg = ETC64.Status(1, 2, ChainWeight(2, 5), ByteString("HASH"), ByteString("HASH2"))
        verify(msg, (m: ETC64.Status) => m.toBytes, Codes.StatusCode, ProtocolVersions.ETC64)
      }
    }

    "encoding and decoding NewBlock" should {
      "return same result for NewBlock v64" in {
        val msg = ETC64.NewBlock(Fixtures.Blocks.Block3125369.block, ChainWeight(2323, 21))
        verify(msg, (m: ETC64.NewBlock) => m.toBytes, Codes.NewBlockCode, ProtocolVersions.ETC64)
      }
    }
  }

  "ETH63" when {
    val version = ProtocolVersions.ETH63
    "encoding and decoding Status" should {
      "return same result for Status v63" in {
        val msg = Status(1, 2, 2, ByteString("HASH"), ByteString("HASH2"))
        verify(msg, (m: Status) => m.toBytes, Codes.StatusCode, ProtocolVersions.ETH63)
      }
    }
    commonEthAssertions(version)
  }

  "ETH64" when {
    val version = ProtocolVersions.ETH64
    "encoding and decoding Status" should {
      "return same result" in {
        val msg = ETH64.Status(1, 2, 3, ByteString("HASH"), ByteString("HASH2"), ForkId(1L, None))
        verify(msg, (m: ETH64.Status) => m.toBytes, Codes.StatusCode, ProtocolVersions.ETH64)
      }
    }
    commonEthAssertions(version)
  }

  //scalastyle:off method.length
  def commonEthAssertions(version: Capability) = {
    "encoding and decoding ETH61.NewBlockHashes" should {
      "throw for unsupported message version" in {
        val msg = ETH61.NewBlockHashes(Seq(ByteString("23"), ByteString("10"), ByteString("36")))
        assertThrows[RuntimeException] {
          verify(msg, (m: ETH61.NewBlockHashes) => m.toBytes, Codes.NewBlockHashesCode, version)
        }
      }
    }

    "encoding and decoding BlockHashesFromNumber" should {
      "return same result" in {
        val msg = BlockHashesFromNumber(1, 2)
        verify(msg, (m: BlockHashesFromNumber) => m.toBytes, Codes.BlockHashesFromNumberCode, version)
      }
    }

    "encoding and decoding ETH62.NewBlockHashes" should {
      "return same result" in {
        val msg = ETH62.NewBlockHashes(Seq(BlockHash(ByteString("hash1"), 1), BlockHash(ByteString("hash2"), 2)))
        verify(msg, (m: ETH62.NewBlockHashes) => m.toBytes, Codes.NewBlockHashesCode, version)
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
  //scalastyle:on

  def verify[T](msg: T, encode: T => Array[Byte], code: Int, version: Capability): Unit =
    messageDecoder(version).fromBytes(code, encode(msg)) shouldEqual msg

  private def messageDecoder(version: Capability) =
    NetworkMessageDecoder.orElse(EthereumMessageDecoder.ethMessageDecoder(version))
}
