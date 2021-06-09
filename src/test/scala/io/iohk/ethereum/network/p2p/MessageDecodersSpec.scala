package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.{Fixtures, ObjectGenerators}
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities._
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.SignedTransactions
import io.iohk.ethereum.network.p2p.messages._
import io.iohk.ethereum.security.SecureRandomBuilder
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MessageDecodersSpec extends AnyFlatSpec with Matchers with SecureRandomBuilder {

  val decode = EthereumMessageDecoder.fromBytes _

  val exampleHash = ByteString(Hex.decode("fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef6"))

  val blockHashesFromNumberBytes: Array[Byte] = Hex.decode("c20c28")

  val NewBlockHashesETH61bytes: Array[Byte] =
    Hex.decode(
      "f842a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef6a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef6"
    )

  "MessageDecoders" should "decode wire protocol message for all versions of protocol" in {
    val helloBytes: Array[Byte] =
      Hex.decode(
        "f85404866d616e746973c6c5836574683f820d05b840a13f3f0555b5037827c743e40fce29139fcf8c3f2a8f12753872fe906a77ff70f6a7f517be995805ff39ab73af1d53dac1a6c9786eebc5935fc455ac8f41ba67"
      )
    val hello = WireProtocol.Hello(
      p2pVersion = 4,
      clientId = "mantis",
      capabilities = Seq(Eth63Capability),
      listenPort = 3333,
      nodeId = ByteString(
        Hex.decode(
          "a13f3f0555b5037827c743e40fce29139fcf8c3f2a8f12753872fe906a77ff70f6a7f517be995805ff39ab73af1d53dac1a6c9786eebc5935fc455ac8f41ba67"
        )
      )
    )
    NetworkMessageDecoder.fromBytes(WireProtocol.Hello.code, helloBytes, ProtocolVersions.ETH61) shouldBe hello
    NetworkMessageDecoder.fromBytes(WireProtocol.Hello.code, helloBytes, ProtocolVersions.ETH62) shouldBe hello
    NetworkMessageDecoder.fromBytes(WireProtocol.Hello.code, helloBytes, ProtocolVersions.ETH63) shouldBe hello
    NetworkMessageDecoder.fromBytes(WireProtocol.Hello.code, helloBytes, ProtocolVersions.ETC64) shouldBe hello
  }

  it should "decode NewBlockHashes message for all supported versions of protocol" in {
    val newBlockHashesETH61 = ETH61.NewBlockHashes(Seq(exampleHash, exampleHash))

    val NewBlockHashesETH62bytes: Array[Byte] =
      Hex.decode(
        "f846e2a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef601e2a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef602"
      )
    val newBlockHashesETH62 = ETH62.NewBlockHashes(Seq(ETH62.BlockHash(exampleHash, 1), ETH62.BlockHash(exampleHash, 2)))

    decode(Codes.NewBlockHashesCode, NewBlockHashesETH61bytes, ProtocolVersions.ETH61) shouldBe newBlockHashesETH61
    decode(Codes.NewBlockHashesCode, NewBlockHashesETH62bytes, ProtocolVersions.ETH62) shouldBe newBlockHashesETH62
    decode(Codes.NewBlockHashesCode, NewBlockHashesETH62bytes, ProtocolVersions.ETH63) shouldBe newBlockHashesETH62
    decode(Codes.NewBlockHashesCode, NewBlockHashesETH62bytes, ProtocolVersions.ETC64) shouldBe newBlockHashesETH62
  }

  it should "not decode message from older version of protocol as newer version" in {
    assertThrows[RuntimeException] {
      decode(Codes.NewBlockHashesCode, NewBlockHashesETH61bytes, ProtocolVersions.ETH62)
    }
  }

  it should "decode BlockHashesFromNumber message for all supported versions of protocol" in {
    val blockHashesFromNumber = ETH61.BlockHashesFromNumber(12, 40)
    decode(
      Codes.BlockHashesFromNumberCode,
      blockHashesFromNumberBytes,
      ProtocolVersions.ETH61
    ) shouldBe blockHashesFromNumber
  }

  it should "decode GetBlockHeaders message for all supported versions of protocol" in {
    val getBlockHeaders = ETH62.GetBlockHeaders(Left(1), 1, 1, false)
    val getBlockHeadersBytes: Array[Byte] = getBlockHeaders.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.GetBlockHeadersCode, getBlockHeadersBytes, ProtocolVersions.ETH61)
    }
    decode(Codes.GetBlockHeadersCode, getBlockHeadersBytes, ProtocolVersions.ETH62) shouldBe getBlockHeaders
    decode(Codes.GetBlockHeadersCode, getBlockHeadersBytes, ProtocolVersions.ETH63) shouldBe getBlockHeaders
    decode(Codes.GetBlockHeadersCode, getBlockHeadersBytes, ProtocolVersions.ETC64) shouldBe getBlockHeaders
  }

  it should "decode BlockHeaders message for all supported versions of protocol" in {
    val blockHeaders = ETH62.BlockHeaders(ObjectGenerators.seqBlockHeaderGen.sample.get)
    val blockHeadersBytes: Array[Byte] = blockHeaders.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.BlockHeadersCode, blockHeadersBytes, ProtocolVersions.ETH61)
    }
    decode(Codes.BlockHeadersCode, blockHeadersBytes, ProtocolVersions.ETH62) shouldBe blockHeaders
    decode(Codes.BlockHeadersCode, blockHeadersBytes, ProtocolVersions.ETH63) shouldBe blockHeaders
    decode(Codes.BlockHeadersCode, blockHeadersBytes, ProtocolVersions.ETC64) shouldBe blockHeaders
  }

  it should "decode GetBlockBodies message for all supported versions of protocol" in {
    val getBlockBodies = ETH62.GetBlockBodies(Seq(exampleHash))
    val getBlockBodiesBytes: Array[Byte] = getBlockBodies.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.GetBlockBodiesCode, getBlockBodiesBytes, ProtocolVersions.ETH61)
    }
    decode(Codes.GetBlockBodiesCode, getBlockBodiesBytes, ProtocolVersions.ETH62) shouldBe getBlockBodies
    decode(Codes.GetBlockBodiesCode, getBlockBodiesBytes, ProtocolVersions.ETH63) shouldBe getBlockBodies
    decode(Codes.GetBlockBodiesCode, getBlockBodiesBytes, ProtocolVersions.ETC64) shouldBe getBlockBodies
  }

  it should "decode BlockBodies message for all supported versions of protocol" in {
    val blockBodies = ETH62.BlockBodies(Seq(Fixtures.Blocks.Block3125369.body, Fixtures.Blocks.DaoForkBlock.body))
    val blockBodiesBytes: Array[Byte] = blockBodies.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.BlockBodiesCode, blockBodiesBytes, ProtocolVersions.ETH61)
    }
    decode(Codes.BlockBodiesCode, blockBodiesBytes, ProtocolVersions.ETH62) shouldBe blockBodies
    decode(Codes.BlockBodiesCode, blockBodiesBytes, ProtocolVersions.ETH63) shouldBe blockBodies
    decode(Codes.BlockBodiesCode, blockBodiesBytes, ProtocolVersions.ETC64) shouldBe blockBodies
  }

  it should "decode GetNodeData message for all supported versions of protocol" in {
    val getNodeData = ETH63.GetNodeData(Seq(exampleHash))
    val getNodeDataBytes: Array[Byte] = getNodeData.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.GetNodeDataCode, getNodeDataBytes, ProtocolVersions.ETH61)
    }
    assertThrows[RuntimeException] {
      decode(Codes.GetNodeDataCode, getNodeDataBytes, ProtocolVersions.ETH62)
    }
    decode(Codes.GetNodeDataCode, getNodeDataBytes, ProtocolVersions.ETH63) shouldBe getNodeData
    decode(Codes.GetNodeDataCode, getNodeDataBytes, ProtocolVersions.ETC64) shouldBe getNodeData
  }

  it should "decode NodeData message for all supported versions of protocol" in {
    val nodeData = ETH63.NodeData(Seq(exampleHash))
    val nodeDataBytes: Array[Byte] = nodeData.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.NodeDataCode, nodeDataBytes, ProtocolVersions.ETH61)
    }
    assertThrows[RuntimeException] {
      decode(Codes.NodeDataCode, nodeDataBytes, ProtocolVersions.ETH62)
    }
    decode(Codes.NodeDataCode, nodeDataBytes, ProtocolVersions.ETH63) shouldBe nodeData
    decode(Codes.NodeDataCode, nodeDataBytes, ProtocolVersions.ETC64) shouldBe nodeData
  }

  it should "decode GetReceipts message for all supported versions of protocol" in {
    val getReceipts = ETH63.GetReceipts(Seq(exampleHash))
    val getReceiptsBytes: Array[Byte] = getReceipts.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.GetReceiptsCode, getReceiptsBytes, ProtocolVersions.ETH61)
    }
    assertThrows[RuntimeException] {
      decode(Codes.GetReceiptsCode, getReceiptsBytes, ProtocolVersions.ETH62)
    }
    decode(Codes.GetReceiptsCode, getReceiptsBytes, ProtocolVersions.ETH63) shouldBe getReceipts
    decode(Codes.GetReceiptsCode, getReceiptsBytes, ProtocolVersions.ETC64) shouldBe getReceipts
  }

  it should "decode Receipts message for all supported versions of protocol" in {
    val receipts = ETH63.Receipts(ObjectGenerators.receiptsGen(3).sample.get)
    val receiptsBytes: Array[Byte] = receipts.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.ReceiptsCode, receiptsBytes, ProtocolVersions.ETH61)
    }
    assertThrows[RuntimeException] {
      decode(Codes.ReceiptsCode, receiptsBytes, ProtocolVersions.ETH62)
    }
    decode(Codes.ReceiptsCode, receiptsBytes, ProtocolVersions.ETH63) shouldBe receipts
    decode(Codes.ReceiptsCode, receiptsBytes, ProtocolVersions.ETC64) shouldBe receipts
  }

  it should "decode Status message for all supported versions of protocol" in {
    val status63 = BaseETH6XMessages.Status(ProtocolVersions.ETH63.version, 1, BigInt(100), exampleHash, exampleHash)
    val status63Bytes: Array[Byte] = status63.toBytes
    val status64 = ETC64.Status(ProtocolVersions.ETH63.version, 1, ChainWeight(1, BigInt(100)), exampleHash, exampleHash)

    // it's not 100 % true as Status message was different in ETH61, but we are not supporting old message
    decode(Codes.StatusCode, status63Bytes, ProtocolVersions.ETH61) shouldBe status63
    decode(Codes.StatusCode, status63Bytes, ProtocolVersions.ETH62) shouldBe status63
    decode(Codes.StatusCode, status63Bytes, ProtocolVersions.ETH63) shouldBe status63
    decode(Codes.StatusCode, status64.toBytes, ProtocolVersions.ETC64) shouldBe status64
  }

  it should "decode NewBlock message for all supported versions of protocol" in {
    val newBlock63 = ObjectGenerators.newBlockGen(secureRandom, None).sample.get
    val newBlock63Bytes: Array[Byte] = newBlock63.toBytes
    val newBlock64 = ObjectGenerators.newBlock64Gen(secureRandom, None).sample.get

    decode(Codes.NewBlockCode, newBlock63Bytes, ProtocolVersions.ETH61) shouldBe newBlock63
    decode(Codes.NewBlockCode, newBlock63Bytes, ProtocolVersions.ETH62) shouldBe newBlock63
    decode(Codes.NewBlockCode, newBlock63Bytes, ProtocolVersions.ETH63) shouldBe newBlock63
    decode(Codes.NewBlockCode, newBlock64.toBytes, ProtocolVersions.ETC64) shouldBe newBlock64
  }

  it should "decode SignedTransactions message for all supported versions of protocol" in {
    val signedTransactions = SignedTransactions(ObjectGenerators.signedTxSeqGen(3, secureRandom, None).sample.get)
    val signedTransactionsBytes: Array[Byte] = signedTransactions.toBytes

    decode(Codes.SignedTransactionsCode, signedTransactionsBytes, ProtocolVersions.ETH61) shouldBe signedTransactions
    decode(Codes.SignedTransactionsCode, signedTransactionsBytes, ProtocolVersions.ETH62) shouldBe signedTransactions
    decode(Codes.SignedTransactionsCode, signedTransactionsBytes, ProtocolVersions.ETH63) shouldBe signedTransactions
    decode(Codes.SignedTransactionsCode, signedTransactionsBytes, ProtocolVersions.ETC64) shouldBe signedTransactions
  }

  it should "not decode message not existing in given protocol" in {
    assertThrows[RuntimeException] {
      decode(Codes.SignedTransactionsCode, blockHashesFromNumberBytes, ProtocolVersions.ETH62)
    }
  }

  it should "not decode message of not supported protocol" in {
    assertThrows[RuntimeException] {
      decode(Codes.NewBlockHashesCode, NewBlockHashesETH61bytes, ProtocolVersions.ETH61.copy(version = 60))
    }
  }
}
