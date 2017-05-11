package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.jsonrpc.EthService.ProtocolVersionRequest
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

class EthServiceSpec extends FlatSpec with Matchers with ScalaFutures {

  behavior of "EthService"

  it should "return ethereum protocol version" in new TestSetup {
    val response = ethService.protocolVersion(ProtocolVersionRequest())
    val protocolVersion = response.futureValue.value

    protocolVersion shouldEqual "0x3f"
    Integer.parseInt(protocolVersion.drop(2), 16) shouldEqual EthService.CurrentProtocolVersion
  }

  trait TestSetup extends MockFactory {
    val appStateStorage = mock[AppStateStorage]
    val ethService = new EthService(appStateStorage)
  }
}
