package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.{Fixtures, ObjectGenerators}
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions
import io.iohk.ethereum.network.p2p.messages._
import io.iohk.ethereum.security.SecureRandomBuilder
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MessageDecodersSpec extends AnyFlatSpec with Matchers with SecureRandomBuilder {

  val decode = EthereumMessageDecoder.fromBytes _

  val exampleHash = ByteString(Hex.decode("fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef6"))

  val blockHashesFromNumberBytes: Array[Byte] = Hex.decode("c20c28")

  val NewBlockHashesPV61bytes: Array[Byte] =
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
    NetworkMessageDecoder.fromBytes(WireProtocol.Hello.code, helloBytes, ProtocolVersions.PV61) shouldBe hello
    NetworkMessageDecoder.fromBytes(WireProtocol.Hello.code, helloBytes, ProtocolVersions.PV62) shouldBe hello
    NetworkMessageDecoder.fromBytes(WireProtocol.Hello.code, helloBytes, ProtocolVersions.PV63) shouldBe hello
    NetworkMessageDecoder.fromBytes(WireProtocol.Hello.code, helloBytes, ProtocolVersions.PV164) shouldBe hello
  }

  it should "decode NewBlockHashes message for all supported versions of protocol" in {
    val newBlockHashesPV61 = PV61.NewBlockHashes(Seq(exampleHash, exampleHash))

    val NewBlockHashesPV62bytes: Array[Byte] =
      Hex.decode(
        "f846e2a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef601e2a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef602"
      )
    val newBlockHashesPV62 = PV62.NewBlockHashes(Seq(PV62.BlockHash(exampleHash, 1), PV62.BlockHash(exampleHash, 2)))

    decode(Codes.NewBlockHashesCode, NewBlockHashesPV61bytes, ProtocolVersions.PV61) shouldBe newBlockHashesPV61
    decode(Codes.NewBlockHashesCode, NewBlockHashesPV62bytes, ProtocolVersions.PV62) shouldBe newBlockHashesPV62
    decode(Codes.NewBlockHashesCode, NewBlockHashesPV62bytes, ProtocolVersions.PV63) shouldBe newBlockHashesPV62
    decode(Codes.NewBlockHashesCode, NewBlockHashesPV62bytes, ProtocolVersions.PV164) shouldBe newBlockHashesPV62
  }

  it should "not decode message from older version of protocol as newer version" in {
    assertThrows[RuntimeException] {
      decode(Codes.NewBlockHashesCode, NewBlockHashesPV61bytes, ProtocolVersions.PV62)
    }
  }

  it should "decode BlockHashesFromNumber message for all supported versions of protocol" in {
    val blockHashesFromNumber = PV61.BlockHashesFromNumber(12, 40)
    decode(
      Codes.BlockHashesFromNumberCode,
      blockHashesFromNumberBytes,
      ProtocolVersions.PV61
    ) shouldBe blockHashesFromNumber
  }

  it should "decode GetBlockHeaders message for all supported versions of protocol" in {
    val getBlockHeaders = PV62.GetBlockHeaders(Left(1), 1, 1, false)
    val getBlockHeadersBytes: Array[Byte] = getBlockHeaders.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.GetBlockHeadersCode, getBlockHeadersBytes, ProtocolVersions.PV61)
    }
    decode(Codes.GetBlockHeadersCode, getBlockHeadersBytes, ProtocolVersions.PV62) shouldBe getBlockHeaders
    decode(Codes.GetBlockHeadersCode, getBlockHeadersBytes, ProtocolVersions.PV63) shouldBe getBlockHeaders
    decode(Codes.GetBlockHeadersCode, getBlockHeadersBytes, ProtocolVersions.PV164) shouldBe getBlockHeaders
  }

  it should "decode BlockHeaders message for all supported versions of protocol" in {
    val blockHeaders = PV62.BlockHeaders(ObjectGenerators.seqBlockHeaderGen.sample.get)
    val blockHeadersBytes: Array[Byte] = blockHeaders.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.BlockHeadersCode, blockHeadersBytes, ProtocolVersions.PV61)
    }
    decode(Codes.BlockHeadersCode, blockHeadersBytes, ProtocolVersions.PV62) shouldBe blockHeaders
    decode(Codes.BlockHeadersCode, blockHeadersBytes, ProtocolVersions.PV63) shouldBe blockHeaders
    decode(Codes.BlockHeadersCode, blockHeadersBytes, ProtocolVersions.PV164) shouldBe blockHeaders
  }

  it should "decode GetBlockBodies message for all supported versions of protocol" in {
    val getBlockBodies = PV62.GetBlockBodies(Seq(exampleHash))
    val getBlockBodiesBytes: Array[Byte] = getBlockBodies.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.GetBlockBodiesCode, getBlockBodiesBytes, ProtocolVersions.PV61)
    }
    decode(Codes.GetBlockBodiesCode, getBlockBodiesBytes, ProtocolVersions.PV62) shouldBe getBlockBodies
    decode(Codes.GetBlockBodiesCode, getBlockBodiesBytes, ProtocolVersions.PV63) shouldBe getBlockBodies
    decode(Codes.GetBlockBodiesCode, getBlockBodiesBytes, ProtocolVersions.PV164) shouldBe getBlockBodies
  }

  it should "decode BlockBodies message for all supported versions of protocol" in {
    val blockBodies = PV62.BlockBodies(Seq(Fixtures.Blocks.Block3125369.body, Fixtures.Blocks.DaoForkBlock.body))
    val blockBodiesBytes: Array[Byte] = blockBodies.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.BlockBodiesCode, blockBodiesBytes, ProtocolVersions.PV61)
    }
    decode(Codes.BlockBodiesCode, blockBodiesBytes, ProtocolVersions.PV62) shouldBe blockBodies
    decode(Codes.BlockBodiesCode, blockBodiesBytes, ProtocolVersions.PV63) shouldBe blockBodies
    decode(Codes.BlockBodiesCode, blockBodiesBytes, ProtocolVersions.PV164) shouldBe blockBodies
  }

  it should "decode GetNodeData message for all supported versions of protocol" in {
    val getNodeData = PV63.GetNodeData(Seq(exampleHash))
    val getNodeDataBytes: Array[Byte] = getNodeData.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.GetNodeDataCode, getNodeDataBytes, ProtocolVersions.PV61)
    }
    assertThrows[RuntimeException] {
      decode(Codes.GetNodeDataCode, getNodeDataBytes, ProtocolVersions.PV62)
    }
    decode(Codes.GetNodeDataCode, getNodeDataBytes, ProtocolVersions.PV63) shouldBe getNodeData
    decode(Codes.GetNodeDataCode, getNodeDataBytes, ProtocolVersions.PV164) shouldBe getNodeData
  }

  it should "decode NodeData message for all supported versions of protocol" in {
    val nodeData = PV63.NodeData(Seq(exampleHash))
    val nodeDataBytes: Array[Byte] = nodeData.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.NodeDataCode, nodeDataBytes, ProtocolVersions.PV61)
    }
    assertThrows[RuntimeException] {
      decode(Codes.NodeDataCode, nodeDataBytes, ProtocolVersions.PV62)
    }
    decode(Codes.NodeDataCode, nodeDataBytes, ProtocolVersions.PV63) shouldBe nodeData
    decode(Codes.NodeDataCode, nodeDataBytes, ProtocolVersions.PV164) shouldBe nodeData
  }

  it should "decode GetReceipts message for all supported versions of protocol" in {
    val getReceipts = PV63.GetReceipts(Seq(exampleHash))
    val getReceiptsBytes: Array[Byte] = getReceipts.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.GetReceiptsCode, getReceiptsBytes, ProtocolVersions.PV61)
    }
    assertThrows[RuntimeException] {
      decode(Codes.GetReceiptsCode, getReceiptsBytes, ProtocolVersions.PV62)
    }
    decode(Codes.GetReceiptsCode, getReceiptsBytes, ProtocolVersions.PV63) shouldBe getReceipts
    decode(Codes.GetReceiptsCode, getReceiptsBytes, ProtocolVersions.PV164) shouldBe getReceipts
  }

  it should "decode Receipts message for all supported versions of protocol" in {
    val receipts = PV63.Receipts(ObjectGenerators.receiptsGen(3).sample.get)
    val receiptsBytes: Array[Byte] = receipts.toBytes

    assertThrows[RuntimeException] {
      decode(Codes.ReceiptsCode, receiptsBytes, ProtocolVersions.PV61)
    }
    assertThrows[RuntimeException] {
      decode(Codes.ReceiptsCode, receiptsBytes, ProtocolVersions.PV62)
    }
    decode(Codes.ReceiptsCode, receiptsBytes, ProtocolVersions.PV63) shouldBe receipts
    decode(Codes.ReceiptsCode, receiptsBytes, ProtocolVersions.PV164) shouldBe receipts
  }

  it should "decode Status message for all supported versions of protocol" in {
    val status63 = CommonMessages.Status(ProtocolVersions.PV63, 1, BigInt(100), exampleHash, exampleHash)
    val status63Bytes: Array[Byte] = status63.toBytes
    val status64 = PV164.Status(ProtocolVersions.PV63, 1, ChainWeight(1, BigInt(100)), exampleHash, exampleHash)

    // it's not 100 % true as Status message was different in PV61, but we are not supporting old message
    decode(Codes.StatusCode, status63Bytes, ProtocolVersions.PV61) shouldBe status63
    decode(Codes.StatusCode, status63Bytes, ProtocolVersions.PV62) shouldBe status63
    decode(Codes.StatusCode, status63Bytes, ProtocolVersions.PV63) shouldBe status63
    decode(Codes.StatusCode, status64.toBytes, ProtocolVersions.PV164) shouldBe status64
  }

  it should "decode NewBlock message for all supported versions of protocol" in {
    val newBlock63 = ObjectGenerators.newBlockGen(secureRandom, None).sample.get
    val newBlock63Bytes: Array[Byte] = newBlock63.toBytes
    val newBlock64 = ObjectGenerators.newBlock64Gen(secureRandom, None).sample.get

    decode(Codes.NewBlockCode, newBlock63Bytes, ProtocolVersions.PV61) shouldBe newBlock63
    decode(Codes.NewBlockCode, newBlock63Bytes, ProtocolVersions.PV62) shouldBe newBlock63
    decode(Codes.NewBlockCode, newBlock63Bytes, ProtocolVersions.PV63) shouldBe newBlock63
    decode(Codes.NewBlockCode, newBlock64.toBytes, ProtocolVersions.PV164) shouldBe newBlock64
  }

  it should "decode SignedTransactions message for all supported versions of protocol" in {
    val signedTransactions = SignedTransactions(ObjectGenerators.signedTxSeqGen(3, secureRandom, None).sample.get)
    val signedTransactionsBytes: Array[Byte] = signedTransactions.toBytes

    decode(Codes.SignedTransactionsCode, signedTransactionsBytes, ProtocolVersions.PV61) shouldBe signedTransactions
    decode(Codes.SignedTransactionsCode, signedTransactionsBytes, ProtocolVersions.PV62) shouldBe signedTransactions
    decode(Codes.SignedTransactionsCode, signedTransactionsBytes, ProtocolVersions.PV63) shouldBe signedTransactions
    decode(Codes.SignedTransactionsCode, signedTransactionsBytes, ProtocolVersions.PV164) shouldBe signedTransactions
  }

  it should "not decode message not existing in given protocol" in {
    assertThrows[RuntimeException] {
      decode(Codes.SignedTransactionsCode, blockHashesFromNumberBytes, ProtocolVersions.PV62)
    }
  }

  it should "not decode message of not supported protocol" in {
    assertThrows[RuntimeException] {
      decode(Codes.NewBlockHashesCode, NewBlockHashesPV61bytes, ProtocolVersions.PV61 - 1)
    }
  }
}
