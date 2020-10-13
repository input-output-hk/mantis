package io.iohk.ethereum.jsonrpc

import java.time.Duration

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.DebugService.{ListPeersInfoRequest, ListPeersInfoResponse}
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.JsonSerializers.{OptionNoneToJNullSerializer, QuantitiesSerializer, UnformattedDataJsonSerializer}
import io.iohk.ethereum.jsonrpc.NetService.{ListeningResponse, PeerCountResponse, VersionResponse}
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer
import io.iohk.ethereum.jsonrpc.server.ipc.JsonRpcIpcServer
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.{Fixtures, LongPatience}
import org.bouncycastle.util.encoders.Hex
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.{DefaultFormats, Extraction, Formats}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.Future
import scala.concurrent.duration._

// scalastyle:off file.size.limit
// scalastyle:off magic.number
class JsonRpcControllerSpec
  extends AnyFlatSpec
    with Matchers
    with JRCMatchers
    with ScalaCheckPropertyChecks
    with ScalaFutures
    with LongPatience
    with Eventually {

  implicit val formats: Formats = DefaultFormats.preservingEmptyValues + OptionNoneToJNullSerializer +
    QuantitiesSerializer + UnformattedDataJsonSerializer

  "JsonRpcController" should "handle valid sha3 request" in new JsonRpcControllerFixture {
    val rpcRequest = newJsonRpcRequest("web3_sha3", JString("0x1234") :: Nil)

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveStringResult("0x56570de287d73cd1cb6092bb8fdee6173974955fdef345ae579ee9f475ea7432")
  }

  it should "fail when invalid request is received" in new JsonRpcControllerFixture {
    val rpcRequest = newJsonRpcRequest("web3_sha3", JString("asdasd") :: Nil)

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveError(JsonRpcErrors.InvalidParams("Invalid method parameters"))
  }

  it should "handle clientVersion request" in new JsonRpcControllerFixture {
    val rpcRequest = newJsonRpcRequest("web3_clientVersion")

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveStringResult(version)
  }

  it should "Handle net_peerCount request" in new JsonRpcControllerFixture {
    (netService.peerCount _).expects(*).returning(Future.successful(Right(PeerCountResponse(123))))

    val rpcRequest = newJsonRpcRequest("net_peerCount")

    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveStringResult("0x7b")
  }

  it should "Handle net_listening request" in new JsonRpcControllerFixture {
    (netService.listening _).expects(*).returning(Future.successful(Right(ListeningResponse(false))))

    val rpcRequest = newJsonRpcRequest("net_listening")
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveBooleanResult(false)
  }

  it should "Handle net_version request" in new JsonRpcControllerFixture {
    val netVersion = "99"

    (netService.version _).expects(*).returning(Future.successful(Right(VersionResponse(netVersion))))

    val rpcRequest = newJsonRpcRequest("net_version")
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveStringResult(netVersion)
  }

  it should "only allow to call methods of enabled apis" in new JsonRpcControllerFixture {
    override def config: JsonRpcConfig = new JsonRpcConfig {
      override val apis = Seq("web3")
      override val accountTransactionsMaxBlocks = 50000
      override def minerActiveTimeout: FiniteDuration = ???
      override def httpServerConfig: JsonRpcHttpServer.JsonRpcHttpServerConfig = ???
      override def ipcServerConfig: JsonRpcIpcServer.JsonRpcIpcServerConfig = ???
    }

    val ethRpcRequest = newJsonRpcRequest("eth_protocolVersion")
    val ethResponse = jsonRpcController.handleRequest(ethRpcRequest).futureValue

    ethResponse should haveError(JsonRpcErrors.MethodNotFound)

    val web3RpcRequest = newJsonRpcRequest("web3_clientVersion")
    val web3Response = jsonRpcController.handleRequest(web3RpcRequest).futureValue

    web3Response should haveStringResult(version)
  }

  it should "personal_importRawKey" in new JsonRpcControllerFixture {
    val key = "7a44789ed3cd85861c0bbf9693c7e1de1862dd4396c390147ecf1275099c6e6f"
    val keyBytes = ByteString(Hex.decode(key))
    val addr = Address("0x00000000000000000000000000000000000000ff")
    val pass = "aaa"

    (personalService.importRawKey _)
      .expects(ImportRawKeyRequest(keyBytes, pass))
      .returning(Future.successful(Right(ImportRawKeyResponse(addr))))

    val params = JString(key) :: JString(pass) :: Nil
    val rpcRequest = newJsonRpcRequest("personal_importRawKey", params)
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveStringResult(addr.toString)
  }

  it should "personal_newAccount" in new JsonRpcControllerFixture {
    val addr = Address("0x00000000000000000000000000000000000000ff")
    val pass = "aaa"

    (personalService.newAccount _)
      .expects(NewAccountRequest(pass))
      .returning(Future.successful(Right(NewAccountResponse(addr))))

    val params = JString(pass) :: Nil
    val rpcRequest = newJsonRpcRequest("personal_newAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveStringResult(addr.toString)
  }

  it should "personal_listAccounts" in new JsonRpcControllerFixture {
    val addresses = List(34, 12391, 123).map(Address(_))
    val pass = "aaa"

    (personalService.listAccounts _)
      .expects(ListAccountsRequest())
      .returning(Future.successful(Right(ListAccountsResponse(addresses))))

    val rpcRequest = newJsonRpcRequest("personal_listAccounts")
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveResult(JArray(addresses.map(a => JString(a.toString))))
  }

  it should "personal_unlockAccount" in new JsonRpcControllerFixture {
    val address = Address(42)
    val pass = "aaa"
    val params = JString(address.toString) :: JString(pass) :: Nil

    (personalService.unlockAccount _)
      .expects(UnlockAccountRequest(address, pass, None))
      .returning(Future.successful(Right(UnlockAccountResponse(true))))

    val rpcRequest = newJsonRpcRequest("personal_unlockAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveBooleanResult(true)
  }

  it should "personal_unlockAccount for specified duration" in new JsonRpcControllerFixture {
    val address = Address(42)
    val pass = "aaa"
    val dur = "1"
    val params = JString(address.toString) :: JString(pass) :: JString(dur) :: Nil

    (personalService.unlockAccount _)
      .expects(UnlockAccountRequest(address, pass, Some(Duration.ofSeconds(1))))
      .returning(Future.successful(Right(UnlockAccountResponse(true))))

    val rpcRequest = newJsonRpcRequest("personal_unlockAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveBooleanResult(true)
  }

  it should "personal_unlockAccount should handle possible duration errors" in new JsonRpcControllerFixture {
    val address = Address(42)
    val pass = "aaa"
    val dur = "alksjdfh"

    val params = JString(address.toString) :: JString(pass) :: JString(dur) :: Nil
    val rpcRequest = newJsonRpcRequest("personal_unlockAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveError(JsonRpcError(-32602, "Invalid method parameters", None))

    val dur2 = Long.MaxValue
    val params2 = JString(address.toString) :: JString(pass) :: JInt(dur2) :: Nil
    val rpcRequest2 = newJsonRpcRequest("personal_unlockAccount", params2)
    val response2 = jsonRpcController.handleRequest(rpcRequest2).futureValue
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
      .returning(Future.successful(Right(UnlockAccountResponse(true))))

    val rpcRequest = newJsonRpcRequest("personal_unlockAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveBooleanResult(true)
  }

  it should "personal_lockAccount" in new JsonRpcControllerFixture {
    val address = Address(42)
    val params = JString(address.toString) :: Nil

    (personalService.lockAccount _)
      .expects(LockAccountRequest(address))
      .returning(Future.successful(Right(LockAccountResponse(true))))

    val rpcRequest = newJsonRpcRequest("personal_lockAccount", params)
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

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
      .returning(Future.successful(Right(SendTransactionWithPassphraseResponse(txHash))))

    val rpcRequest = newJsonRpcRequest("personal_sendTransaction", params)
    val response = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveResult(JString(s"0x${Hex.toHexString(txHash.toArray)}"))
  }

  it should "debug_listPeersInfo" in new JsonRpcControllerFixture {
    val peerStatus = Status(
      protocolVersion = Versions.PV63,
      networkId = 1,
      totalDifficulty = BigInt("10000"),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      totalDifficulty = peerStatus.totalDifficulty,
      forkAccepted = true,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number,
      bestBlockHash = peerStatus.bestHash
    )
    val peers = List(initialPeerInfo)

    (debugService.listPeersInfo _)
      .expects(ListPeersInfoRequest())
      .returning(Future.successful(Right(ListPeersInfoResponse(peers))))

    val rpcRequest = newJsonRpcRequest("debug_listPeersInfo")
    val response: JsonRpcResponse = jsonRpcController.handleRequest(rpcRequest).futureValue

    response should haveResult(JArray(peers.map(info => JString(info.toString))))
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
      .returns(Future.successful(Right(SignResponse(sig))))

    val request: JsonRpcRequest = newJsonRpcRequest(
      "personal_sign",
      List(
        JString(s"0xdeadbeaf"),
        JString(s"0x9b2055d370f73ec7d8a03e965129118dc8f5bf83"),
        JString("thePassphrase")
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult(
      "0xa3f20717a250c2b0b729b7e5becbff67fdaef7e0699da4de7ca5895b02a170a12d887fd3b17bfdce3481f10bea41f45ba9f709d39ce8325427b57afcfc994cee1b"
    )
  }

  it should "personal_ecRecover" in new JsonRpcControllerFixture {

    (personalService.ecRecover _)
      .expects(EcRecoverRequest(ByteString(Hex.decode("deadbeaf")), sig))
      .returns(
        Future.successful(
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

    val response = jsonRpcController.handleRequest(request).futureValue
    response should haveStringResult("0x9b2055d370f73ec7d8a03e965129118dc8f5bf83")
  }

  it should "rpc_modules" in new JsonRpcControllerFixture {
    val request: JsonRpcRequest = newJsonRpcRequest("rpc_modules")

    val response = jsonRpcController.handleRequest(request).futureValue

    response should haveResult(
      JObject(
        "net" -> "1.0",
        "rpc" -> "1.0",
        "personal" -> "1.0",
        "eth" -> "1.0",
        "web3" -> "1.0",
        "daedalus" -> "1.0",
        "debug" -> "1.0",
        "qa" -> "1.0"
      )
    )
  }

  it should "daedalus_getAccountTransactions" in new JsonRpcControllerFixture {
    val mockEthService: EthService = mock[EthService]
    override val jsonRpcController = newJsonRpcController(mockEthService)

    val block = Fixtures.Blocks.Block3125369
    val sentTx = block.body.transactionList.head
    val receivedTx = block.body.transactionList.last

    (mockEthService.getAccountTransactions _)
      .expects(*)
      .returning(
        Future.successful(
          Right(
            GetAccountTransactionsResponse(
              Seq(
                TransactionResponse(sentTx, Some(block.header), isOutgoing = Some(true)),
                TransactionResponse(receivedTx, Some(block.header), isOutgoing = Some(false))
              )
            )
          )
        )
      )

    val request: JsonRpcRequest = newJsonRpcRequest(
      "daedalus_getAccountTransactions",
      List(
        JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
        JInt(100),
        JInt(200)
      )
    )

    val response = jsonRpcController.handleRequest(request).futureValue
    val expectedTxs = Seq(
      Extraction.decompose(TransactionResponse(sentTx, Some(block.header), isOutgoing = Some(true))),
      Extraction.decompose(TransactionResponse(receivedTx, Some(block.header), isOutgoing = Some(false)))
    )

    response should haveObjectResult("transactions" -> JArray(expectedTxs.toList))
  }
}
