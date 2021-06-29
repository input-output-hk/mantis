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

  def decode: Capability => MessageDecoder = EthereumMessageDecoder.ethMessageDecoder _

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
    NetworkMessageDecoder.fromBytes(WireProtocol.Hello.code, helloBytes) shouldBe hello
  }

  it should "decode NewBlockHashes message for all supported versions of protocol" in {
    val NewBlockHashesETH62bytes: Array[Byte] =
      Hex.decode(
        "f846e2a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef601e2a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef602"
      )
    val newBlockHashesETH62 =
      ETH62.NewBlockHashes(Seq(ETH62.BlockHash(exampleHash, 1), ETH62.BlockHash(exampleHash, 2)))

    decode(ProtocolVersions.ETH63)
      .fromBytes(Codes.NewBlockHashesCode, NewBlockHashesETH62bytes) shouldBe newBlockHashesETH62
    decode(ProtocolVersions.ETC64)
      .fromBytes(Codes.NewBlockHashesCode, NewBlockHashesETH62bytes) shouldBe newBlockHashesETH62
  }

  it should "not decode message from older version of protocol as newer version" in {
    assertThrows[RuntimeException] {
      decode(ProtocolVersions.ETH62).fromBytes(Codes.NewBlockHashesCode, NewBlockHashesETH61bytes)
    }
  }

  it should "decode BlockHashesFromNumber message for all supported versions of protocol" in {
    val blockHashesFromNumber = ETH61.BlockHashesFromNumber(12, 40)
    decode(ProtocolVersions.ETH63).fromBytes(
      Codes.BlockHashesFromNumberCode,
      blockHashesFromNumberBytes
    ) shouldBe blockHashesFromNumber
  }

  it should "decode GetBlockHeaders message for all supported versions of protocol" in {
    val getBlockHeaders = ETH62.GetBlockHeaders(Left(1), 1, 1, false)
    val getBlockHeadersBytes: Array[Byte] = getBlockHeaders.toBytes

    decode(ProtocolVersions.ETH63).fromBytes(Codes.GetBlockHeadersCode, getBlockHeadersBytes) shouldBe getBlockHeaders
    decode(ProtocolVersions.ETC64).fromBytes(Codes.GetBlockHeadersCode, getBlockHeadersBytes) shouldBe getBlockHeaders
  }

  it should "decode BlockHeaders message for all supported versions of protocol" in {
    val blockHeaders = ETH62.BlockHeaders(ObjectGenerators.seqBlockHeaderGen.sample.get)
    val blockHeadersBytes: Array[Byte] = blockHeaders.toBytes

    decode(ProtocolVersions.ETH63).fromBytes(Codes.BlockHeadersCode, blockHeadersBytes) shouldBe blockHeaders
    decode(ProtocolVersions.ETC64).fromBytes(Codes.BlockHeadersCode, blockHeadersBytes) shouldBe blockHeaders
  }

  it should "decode GetBlockBodies message for all supported versions of protocol" in {
    val getBlockBodies = ETH62.GetBlockBodies(Seq(exampleHash))
    val getBlockBodiesBytes: Array[Byte] = getBlockBodies.toBytes

    decode(ProtocolVersions.ETH63).fromBytes(Codes.GetBlockBodiesCode, getBlockBodiesBytes) shouldBe getBlockBodies
    decode(ProtocolVersions.ETC64).fromBytes(Codes.GetBlockBodiesCode, getBlockBodiesBytes) shouldBe getBlockBodies
  }

  it should "decode BlockBodies message for all supported versions of protocol" in {
    val blockBodies = ETH62.BlockBodies(Seq(Fixtures.Blocks.Block3125369.body, Fixtures.Blocks.DaoForkBlock.body))
    val blockBodiesBytes: Array[Byte] = blockBodies.toBytes

    decode(ProtocolVersions.ETH63).fromBytes(Codes.BlockBodiesCode, blockBodiesBytes) shouldBe blockBodies
    decode(ProtocolVersions.ETC64).fromBytes(Codes.BlockBodiesCode, blockBodiesBytes) shouldBe blockBodies
  }

  it should "decode GetNodeData message for all supported versions of protocol" in {
    val getNodeData = ETH63.GetNodeData(Seq(exampleHash))
    val getNodeDataBytes: Array[Byte] = getNodeData.toBytes

    decode(ProtocolVersions.ETH63).fromBytes(Codes.GetNodeDataCode, getNodeDataBytes) shouldBe getNodeData
    decode(ProtocolVersions.ETC64).fromBytes(Codes.GetNodeDataCode, getNodeDataBytes) shouldBe getNodeData
  }

  it should "decode NodeData message for all supported versions of protocol" in {
    val nodeData = ETH63.NodeData(Seq(exampleHash))
    val nodeDataBytes: Array[Byte] = nodeData.toBytes

    decode(ProtocolVersions.ETH63).fromBytes(Codes.NodeDataCode, nodeDataBytes) shouldBe nodeData
    decode(ProtocolVersions.ETC64).fromBytes(Codes.NodeDataCode, nodeDataBytes) shouldBe nodeData
  }

  it should "decode GetReceipts message for all supported versions of protocol" in {
    val getReceipts = ETH63.GetReceipts(Seq(exampleHash))
    val getReceiptsBytes: Array[Byte] = getReceipts.toBytes

    decode(ProtocolVersions.ETH63).fromBytes(Codes.GetReceiptsCode, getReceiptsBytes) shouldBe getReceipts
    decode(ProtocolVersions.ETC64).fromBytes(Codes.GetReceiptsCode, getReceiptsBytes) shouldBe getReceipts
  }

  it should "decode Receipts message for all supported versions of protocol" in {
    val receipts = ETH63.Receipts(ObjectGenerators.receiptsGen(3).sample.get)
    val receiptsBytes: Array[Byte] = receipts.toBytes

    decode(ProtocolVersions.ETH63).fromBytes(Codes.ReceiptsCode, receiptsBytes) shouldBe receipts
    decode(ProtocolVersions.ETC64).fromBytes(Codes.ReceiptsCode, receiptsBytes) shouldBe receipts
  }

  it should "decode Status message for all supported versions of protocol" in {
    val status63 = BaseETH6XMessages.Status(ProtocolVersions.ETH63.version, 1, BigInt(100), exampleHash, exampleHash)
    val status63Bytes: Array[Byte] = status63.toBytes
    val status64 =
      ETC64.Status(ProtocolVersions.ETH63.version, 1, ChainWeight(1, BigInt(100)), exampleHash, exampleHash)

    decode(ProtocolVersions.ETH63).fromBytes(Codes.StatusCode, status63Bytes) shouldBe status63
    decode(ProtocolVersions.ETC64).fromBytes(Codes.StatusCode, status64.toBytes) shouldBe status64
  }

  it should "decode NewBlock message for all supported versions of protocol" in {
    val newBlock63 = ObjectGenerators.newBlockGen(secureRandom, None).sample.get
    val newBlock63Bytes: Array[Byte] = newBlock63.toBytes
    val newBlock64 = ObjectGenerators.newBlock64Gen(secureRandom, None).sample.get

    decode(ProtocolVersions.ETH63).fromBytes(Codes.NewBlockCode, newBlock63Bytes) shouldBe newBlock63
    decode(ProtocolVersions.ETC64).fromBytes(Codes.NewBlockCode, newBlock64.toBytes) shouldBe newBlock64
  }

  it should "decode SignedTransactions message for all supported versions of protocol" in {
    val signedTransactions = SignedTransactions(ObjectGenerators.signedTxSeqGen(3, secureRandom, None).sample.get)
    val signedTransactionsBytes: Array[Byte] = signedTransactions.toBytes

    decode(ProtocolVersions.ETH63)
      .fromBytes(Codes.SignedTransactionsCode, signedTransactionsBytes) shouldBe signedTransactions
    decode(ProtocolVersions.ETC64)
      .fromBytes(Codes.SignedTransactionsCode, signedTransactionsBytes) shouldBe signedTransactions
  }

  it should "not decode message not existing in given protocol" in {
    assertThrows[RuntimeException] {
      decode(ProtocolVersions.ETH63).fromBytes(Codes.SignedTransactionsCode, blockHashesFromNumberBytes)
    }
  }

  it should "not decode message of not supported protocol" in {
    assertThrows[RuntimeException] {
      decode(ProtocolVersions.ETH61).fromBytes(Codes.NewBlockHashesCode, NewBlockHashesETH61bytes)
    }
  }
}
