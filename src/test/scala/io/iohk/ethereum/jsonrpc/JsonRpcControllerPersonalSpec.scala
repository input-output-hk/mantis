package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers.{
  OptionNoneToJNullSerializer,
  QuantitiesSerializer,
  UnformattedDataJsonSerializer
}
import io.iohk.ethereum.{LongPatience, WithActorSystemShutDown}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.bouncycastle.util.encoders.Hex
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.{DefaultFormats, Formats}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Duration

class JsonRpcControllerPersonalSpec
    extends TestKit(ActorSystem("JsonRpcControllerPersonalSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with JRCMatchers
    with ScalaCheckPropertyChecks
    with ScalaFutures
    with LongPatience
    with Eventually {

  implicit val formats: Formats = DefaultFormats.preservingEmptyValues + OptionNoneToJNullSerializer +
    QuantitiesSerializer + UnformattedDataJsonSerializer

  it should "personal_importRawKey" in new JsonRpcControllerFixture {
    val key = "7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"
    val keyBytes = ByteString(Hex.decode(key))
    val addr = Address("0x00000000000000000000000000000000000000ff")
    val pass = "aaa"

    (personalService.importRawKey _)
      .expects(ImportRawKeyRequest(keyBytes, pass))
      .returning(Task.now(Right(ImportRawKeyResponse(addr))))

    val params = JString(key) :: JString(pass) :: Nil
    val rpcRequest = newJsonRpcRequest("personal_importRawKey", params)
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveStringResult(addr.toString)
  }

  it should "personal_newAccount" in new JsonRpcControllerFixture {
    val addr = Address("0x00000000000000000000000000000000000000ff")
    val pass = "aaa"

    (personalService.newAccount _)
      .expects(NewAccountRequest(pass))
      .returning(Task.now(Right(NewAccountResponse(addr))))

    val params = JString(pass) :: Nil
    val rpcRequest = newJsonRpcRequest("personal_newAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveStringResult(addr.toString)
  }

  it should "personal_listAccounts" in new JsonRpcControllerFixture {
    val addresses = List(34, 12391, 123).map(Address(_))
    val pass = "aaa"

    (personalService.listAccounts _)
      .expects(ListAccountsRequest())
      .returning(Task.now(Right(ListAccountsResponse(addresses))))

    val rpcRequest = newJsonRpcRequest("personal_listAccounts")
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveResult(JArray(addresses.map(a => JString(a.toString))))
  }

  it should "personal_unlockAccount" in new JsonRpcControllerFixture {
    val address = Address(42)
    val pass = "aaa"
    val params = JString(address.toString) :: JString(pass) :: Nil

    (personalService.unlockAccount _)
      .expects(UnlockAccountRequest(address, pass, None))
      .returning(Task.now(Right(UnlockAccountResponse(true))))

    val rpcRequest = newJsonRpcRequest("personal_unlockAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveBooleanResult(true)
  }

  it should "personal_unlockAccount for specified duration" in new JsonRpcControllerFixture {
    val address = Address(42)
    val pass = "aaa"
    val dur = "1"
    val params = JString(address.toString) :: JString(pass) :: JString(dur) :: Nil

    (personalService.unlockAccount _)
      .expects(UnlockAccountRequest(address, pass, Some(Duration.ofSeconds(1))))
      .returning(Task.now(Right(UnlockAccountResponse(true))))

    val rpcRequest = newJsonRpcRequest("personal_unlockAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveBooleanResult(true)
  }

  it should "personal_unlockAccount should handle possible duration errors" in new JsonRpcControllerFixture {
    val address = Address(42)
    val pass = "aaa"
    val dur = "alksjdfh"

    val params = JString(address.toString) :: JString(pass) :: JString(dur) :: Nil
    val rpcRequest = newJsonRpcRequest("personal_unlockAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveError(JsonRpcError(-32602, "Invalid method parameters", None))

    val dur2 = Long.MaxValue
    val params2 = JString(address.toString) :: JString(pass) :: JInt(dur2) :: Nil
    val rpcRequest2 = newJsonRpcRequest("personal_unlockAccount", params2)
    val response2 = jsonRpcController.handleRequest(rpcRequest2).runSyncUnsafe()
    response2 should haveError(
      JsonRpcError(-32602, "Duration should be an number of seconds, less than 2^31 - 1", None)
    )
  }

  it should "personal_unlockAccount should handle null passed as a duration for compatibility with Parity and web3j" in new JsonRpcControllerFixture {
    val address = Address(42)
    val pass = "aaa"
    val params = JString(address.toString) :: JString(pass) :: JNull :: Nil

    (personalService.unlockAccount _)
      .expects(UnlockAccountRequest(address, pass, None))
      .returning(Task.now(Right(UnlockAccountResponse(true))))

    val rpcRequest = newJsonRpcRequest("personal_unlockAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveBooleanResult(true)
  }

  it should "personal_lockAccount" in new JsonRpcControllerFixture {
    val address = Address(42)
    val params = JString(address.toString) :: Nil

    (personalService.lockAccount _)
      .expects(LockAccountRequest(address))
      .returning(Task.now(Right(LockAccountResponse(true))))

    val rpcRequest = newJsonRpcRequest("personal_lockAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveBooleanResult(true)
  }

  it should "personal_sendTransaction" in new JsonRpcControllerFixture {
    val params = JObject(
      "from" -> Address(42).toString,
      "to" -> Address(123).toString,
      "value" -> 1000
    ) :: JString("passphrase") :: Nil

    val txHash = ByteString(1, 2, 3, 4)

    (personalService
      .sendTransaction(_: SendTransactionWithPassphraseRequest))
      .expects(*)
      .returning(Task.now(Right(SendTransactionWithPassphraseResponse(txHash))))

    val rpcRequest = newJsonRpcRequest("personal_sendTransaction", params)
    val response = jsonRpcController.handleRequest(rpcRequest).runSyncUnsafe()

    response should haveResult(JString(s"0x${Hex.toHexString(txHash.toArray)}"))
  }

  it should "personal_sign" in new JsonRpcControllerFixture {

    (personalService.sign _)
      .expects(
        SignRequest(
          ByteString(Hex.decode("deadbeaf")),
          Address(ByteString(Hex.decode("9b2055d370f73ec7d8a03e965129118dc8f5bf83"))),
          Some("thePassphrase")
        )
      )
      .returns(Task.now(Right(SignResponse(sig))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "personal_sign",
      List(
        JString(s"0xdeadbeaf"),
        JString(s"0x9b2055d370f73ec7d8a03e965129118dc8f5bf83"),
        JString("thePassphrase")
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult(
      "0xa3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a12d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee1b"
    )
  }

  it should "personal_ecRecover" in new JsonRpcControllerFixture {

    (personalService.ecRecover _)
      .expects(EcRecoverRequest(ByteString(Hex.decode("deadbeaf")), sig))
      .returns(
        Task.now(
          Right(EcRecoverResponse(Address(ByteString(Hex.decode("9b2055d370f73ec7d8a03e965129118dc8f5bf83")))))
        )
      )

    val request: JsonRpcRequest = newJsonRpcRequest(
      "personal_ecRecover",
      List(
        JString(s"0xdeadbeaf"),
        JString(
          s"0xa3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a12d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee1b"
        )
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveStringResult("0x9b2055d370f73ec7d8a03e965129118dc8f5bf83")
  }
}
