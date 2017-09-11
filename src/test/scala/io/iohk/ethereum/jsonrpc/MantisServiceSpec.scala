package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.MantisService.ImportMnemonicRequest
import io.iohk.ethereum.jsonrpc.PersonalService.{ImportRawKeyRequest, ImportRawKeyResponse}
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.Promise

class MantisServiceSpec extends FlatSpec with Matchers with MockFactory with ScalaFutures  {

  "PersonalService" should "import private keys" in new TestSetup {

    val address = Address("0xc05738de2c42ec7bce2029d4624b0b5583da6719")
    (personal.importRawKey _).when(*).returning(Promise.successful(Right[JsonRpcError, ImportRawKeyResponse](ImportRawKeyResponse(address))).future)

    val req = ImportMnemonicRequest("legal winner thank year wave sausage worth useful legal sausage worth title", "1q2w3e4r")
    val res = mantis.importMnemonic(req).futureValue

    res shouldEqual Right(ImportRawKeyResponse(address))
  }

  "PersonalService" should "import same private key if using same mnemonic" in new TestSetup {

    (personal.importRawKey _)
      .when(*)
      .twice()
      .returning(Promise.successful(Right[JsonRpcError, ImportRawKeyResponse](ImportRawKeyResponse(Address(1)))).future)

    val req = ImportMnemonicRequest("ethereum classic", "1q2w3e4r")
    mantis.importMnemonic(req).futureValue
    mantis.importMnemonic(req).futureValue
    (personal.importRawKey _).verify(
      ImportRawKeyRequest(
        ByteString(Hex.decode("b6e66f8d87b3dc8be4d1b51b2076a8583c3a50933b05e603d77ac5b35f63cb97")),
        "1q2w3e4r"
      )
    ).twice()
  }


  trait TestSetup {

    val personal = stub[PersonalService]
    val mantis = new MantisService(personal)

  }
}
