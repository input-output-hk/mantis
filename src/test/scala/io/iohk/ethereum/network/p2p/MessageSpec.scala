package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.messages._
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class MessageSpec extends FlatSpec with Matchers {

  val decode = EthereumMessageDecoder.fromBytes _

  val exampleHash = ByteString(Hex.decode("fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef6"))

  val NewBlockHashesPV61bytes: Array[Byte] =
    Hex.decode("f842a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef6a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef6")
  val newBlockHashesPV61 = PV61.NewBlockHashes(Seq(exampleHash, exampleHash))

  val NewBlockHashesPV62bytes: Array[Byte] =
    Hex.decode("f846e2a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef601e2a0fccdbfe911f9df0a6cc0107d1240f76dfdd1d301b65fdc3cd2ae62752affbef602")
  val newBlockHashesPV62 = PV62.NewBlockHashes(Seq(PV62.BlockHash(exampleHash, 1), PV62.BlockHash(exampleHash, 2)))

  val BlockHashesFromNumberBytes: Array[Byte] = Hex.decode("c20c28")
  val blockHashesFromNumber = PV61.BlockHashesFromNumber(12, 40)

  val helloBytes: Array[Byte] =
    Hex.decode("f858048a6574632d636c69656e74c6c5836574683f820d05b840a13f3f0555b5037827c743e40fce29139fcf8c3f2a8f12753872fe906a77ff70f6a7f517be995805ff39ab73af1d53dac1a6c9786eebc5935fc455ac8f41ba67")
  val hello = WireProtocol.Hello(
    p2pVersion = 4,
    clientId = "mantis",
    capabilities = Seq(WireProtocol.Capability("eth", Versions.PV63.toByte)),
    listenPort = 3333,
    nodeId = ByteString(Hex.decode("a13f3f0555b5037827c743e40fce29139fcf8c3f2a8f12753872fe906a77ff70f6a7f517be995805ff39ab73af1d53dac1a6c9786eebc5935fc455ac8f41ba67")))

  "Message" should "decode message from given version of protocol" in {
    decode(PV61.NewBlockHashes.code, NewBlockHashesPV61bytes, Versions.PV61) shouldBe newBlockHashesPV61
  }

  it should "decode message redefined in newer version of protocol" in {
    decode(PV62.NewBlockHashes.code, NewBlockHashesPV62bytes, Versions.PV62) shouldBe newBlockHashesPV62
    decode(PV62.NewBlockHashes.code, NewBlockHashesPV62bytes, Versions.PV63) shouldBe newBlockHashesPV62
  }

  it should "decode message available only in older version of protocol" in {
    decode(PV61.BlockHashesFromNumber.code, BlockHashesFromNumberBytes, Versions.PV61) shouldBe blockHashesFromNumber
  }

  it should "not decode message from older version of protocol as newer version" in {
    assertThrows[RuntimeException] {
      decode(PV62.NewBlockHashes.code, NewBlockHashesPV61bytes, Versions.PV62)
    }
  }

  it should "not decode message not existing in given protocol" in {
    assertThrows[RuntimeException] {
      decode(CommonMessages.SignedTransactions.code, BlockHashesFromNumberBytes, Versions.PV62)
    }
  }

  it should "decode wire protocol message for all versions of protocol" in {
    decode(WireProtocol.Hello.code, helloBytes, Versions.PV61) shouldBe hello
    decode(WireProtocol.Hello.code, helloBytes, Versions.PV62) shouldBe hello
    decode(WireProtocol.Hello.code, helloBytes, Versions.PV63) shouldBe hello
  }
}
