package io.iohk.ethereum.rpcTest

import java.math.BigInteger
import java.security.SecureRandom

import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.util.ByteString
import cats.effect.Resource
import com.typesafe.config.ConfigFactory
import io.circe.parser.parse
import io.circe.{Decoder, HCursor, Json}
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.TransactionRequest
import io.iohk.ethereum.keystore.{KeyStoreImpl, Wallet}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rpcTest.data.TestContracts._
import io.iohk.ethereum.rpcTest.data.TestData._
import io.iohk.ethereum.utils.KeyStoreConfig
import monix.eval.Task
import monix.reactive.Observable
import org.bouncycastle.util.encoders.Hex
import org.web3j.crypto.Sign.SignatureData
import org.web3j.crypto.{Credentials, Sign}
import org.web3j.protocol.admin.Admin
import org.web3j.protocol.core.DefaultBlockParameter
import org.web3j.protocol.core.methods.request.Transaction
import org.web3j.protocol.core.methods.response.{EthLog, TransactionReceipt}
import org.web3j.protocol.core.methods.response.EthLog.LogObject
import org.web3j.protocol.http.HttpService
import io.iohk.ethereum.utils.FunctorOps._
import cats.instances.future._
import monix.execution.Scheduler

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.concurrent.duration._

abstract class ScenarioSetup {
  implicit val scheduler: Scheduler = monix.execution.Scheduler(scala.concurrent.ExecutionContext.global)

  val testConfig = RpcTestConfig("test.conf")

  val mantisConfig = ConfigFactory.load("application.conf").getConfig("mantis")
  val clientVersion: String = mantisConfig.getString("client-version")
  val protocolVersion = mantisConfig.getConfig("network").getInt("protocol-version")
  val mantisUrl = testConfig.mantisUrl
  //

  val service = Admin.build(new HttpService(mantisUrl))
  val unexisitingBlockHash = "0xaaaaaaaaaaa959b3db6469104c59b803162cf37a23293e8df306e559218f5c6f"
  val badHash = "0xm"
  val emptyResponse = "0x"
  val generalErrorCode = -32602

  val futureBlock = DefaultBlockParameter.valueOf(BigInt(50000000000000L).bigInteger)
  val latestBlock = DefaultBlockParameter.valueOf("latest")
  val pendingBlock = DefaultBlockParameter.valueOf("pending")
  val earliestBlock = DefaultBlockParameter.valueOf("earliest")

  def getBlockParam(number: BigInt): DefaultBlockParameter = {
    DefaultBlockParameter.valueOf(number)
  }

  def getLogs(logResponse: EthLog): List[LogObject] = {
    logResponse.getLogs.asScala.toList.map(log => log.asInstanceOf[LogObject])
  }

  val decode = utils.decode _

  val hexToBigInt = utils.hexToBigInt _

  implicit class BigIntegerExt(val x: BigInteger) {
    def asBigInt: BigInt = BigInt(x)
  }
  implicit def intToBigInt(x: Int): BigInteger = BigInt(x).bigInteger
  implicit def BigIntToBingInteger(x: BigInt): BigInteger = x.bigInteger

  // Helpers to provide some meaningful naming in tests
  def createContract(address: String, code: String, gasLimit: Option[BigInteger] = None): Transaction = {
    new Transaction (
      address,
      null,
      null,
      gasLimit.orNull,
      null,
      null,
      code
    )
  }

  def contractCall(address: String, to: String, data: String, gasLimit: Option[BigInteger] = None): Transaction = {
    new Transaction (
      address,
      null,
      null,
      gasLimit.orNull,
      to,
      null,
      data
    )
  }

  def valueTransfer(from: String, to: String, amount: BigInt, nonce: Option[BigInteger] = None, gasLimit: Option[BigInteger] = None): Transaction = {
    new Transaction (
      from,
      nonce.orNull,
      null,
      gasLimit.orNull,
      to,
      amount,
      null
    )
  }

  val sampleTransaction = createContract(firstAccount.address, testContract)

  // helper to setup two accounts with same nonce and some initial funding
  def setupTwoNewAccounts(fundsProvider: String, amount: BigInt): (TestAccount, TestAccount) = {
    val first = service.personalNewAccount("").send().getAccountId
    val second = service.personalNewAccount("").send().getAccountId

    val firstUnlock = service.personalUnlockAccount(first, "", 0).send()
    val secondUnlock = service.personalUnlockAccount(second, "", 0).send()

    val trans = service.ethSendTransaction(valueTransfer(fundsProvider, first, amount)).send()
    val trans1 = service.ethSendTransaction(valueTransfer(fundsProvider, second, amount)).send()

    // wait for mine
    val block = service.blockObservable(false).toBlocking().first()

    (TestAccount(first, "", amount), TestAccount(second, "", amount))
  }

  // Needed to sign transaction and send raw transactions
  val keyStoreConfig = KeyStoreConfig.customKeyStoreConfig(testConfig.keystoreDir)

  val keyStore = new KeyStoreImpl(keyStoreConfig, new SecureRandom())

  def getAccountWallet(address: String, pass: String): Wallet = {
    keyStore.unlockAccount(Address(address), pass) match {
      case Right(w) => w
      case Left(err) => throw new RuntimeException(s"Cannot get wallet, because of $err")
    }
  }

  def prepareRawTx(
    fromAccount: TestAccount,
    toAccount: Option[TestAccount] = None,
    value: Option[BigInt] = None,
    data: Option[ByteString] = None,
    nonce: BigInt): String = {
    val fromAddress = Address(fromAccount.address)
    val fromWallet = getAccountWallet(fromAccount.address, fromAccount.password)

    val req = TransactionRequest(
      from = fromAddress,
      to = toAccount.map(acc => Address(acc.address)),
      value = value,
      data = data,
      nonce = Some(nonce))

    val transaction = req.toTransaction(0)

    val stx = fromWallet.signTx(transaction, None)
    Hex.toHexString(rlp.encode(stx.tx.toRLPEncodable))
  }

  def waitForTransaction(hash: String): Task[TransactionReceipt] = {
    assert(hash != null, "Transaction hash cannot be null")
    Observable
      .interval(1.second)
      .map(_ => service.ethGetTransactionReceipt(hash).send())
      .map(_.getTransactionReceipt)
      .filter(_.isPresent)
      .map(_.get)
      .firstL
  }

  def unlockAccount(acc: TestAccount): Task[Unit] = {
    Task {
      service.personalUnlockAccount(acc.address, acc.password).send()
    }
  }
  def unlockAccount(acc: TestAccount, duration: FiniteDuration): Task[Unit] = {
    Task {
      service.personalUnlockAccount(acc.address, acc.password, new BigInteger(duration.toSeconds.toString)).send()
    }
  }

}

trait RawHttpRpc { self: ScenarioSetup =>
  import RawHttpRpc._
  def makeHttp(): Resource[Task, RawHttpRpc.HttpService] = Resource.make {
    Task {
      implicit val actorSystem: ActorSystem = ActorSystem()
      implicit val actorMaterializer: ActorMaterializer = ActorMaterializer()

      val http = Http()

      new RawHttpRpc.HttpService(mantisUrl, http, actorSystem, actorMaterializer)
    }
  } { httpService => Task { httpService.actorSystem.terminate()}}

  implicit val actorSystem = ActorSystem()
  implicit val actorMaterializer = ActorMaterializer()

  val oldHttp = Http()

  def call(methodName: String)(implicit ec: ExecutionContext): Future[HCursor] = call(methodName, Nil)

  def call[A: RpcParam](methodName: String, param: A)(implicit ec: ExecutionContext): Future[HCursor] =
    call(methodName, List(RpcParam[A].serialize(param)))

  def call[A: RpcParam, B: RpcParam](methodName: String, param1: A, param2: B)(implicit ec: ExecutionContext): Future[HCursor] =
    call(methodName, List(RpcParam[A].serialize(param1), RpcParam[B].serialize(param2)))

  def call[A: RpcParam, B: RpcParam, C: RpcParam](
                                                   methodName: String,
                                                   param1: A,
                                                   param2: B,
                                                   param3: C)(implicit ec: ExecutionContext): Future[HCursor] =
    call(
      methodName,
      List(RpcParam[A].serialize(param1), RpcParam[B].serialize(param2), RpcParam[C].serialize(param3)))

  private def call(methodName: String, params: List[String])(implicit ec: ExecutionContext): Future[HCursor] = {
    oldHttp
      .singleRequest(buildRequest(mantisUrl, methodName, params))
      .flatMap(res => Unmarshal(res.entity).to[String])
      .tap(res => println(s"Unmarshalled: $res"))
      .map(parse(_).getOrElse(Json.Null).hcursor)
  }

  def getResult[T: Decoder](res: Json): Decoder.Result[T] = res.hcursor.downField("result").as[T]

  def getErrorCode(res: Json): Decoder.Result[Int] = res.hcursor.downField("error").downField("code").as[Int]

  def terminate(): Unit =
    actorSystem.terminate()
}
object RawHttpRpc {
  class HttpService(mantisUrl: String, val http: HttpExt, val actorSystem: ActorSystem, implicit val actorMaterializer: ActorMaterializer) {
    def call(methodName: String): Task[Json] = call(methodName, Nil)

    def call[A: RpcParam](methodName: String, param: A): Task[Json] =
      call(methodName, List(RpcParam[A].serialize(param)))

    def call[A: RpcParam, B: RpcParam](methodName: String, param1: A, param2: B): Task[Json] =
      call(methodName, List(RpcParam[A].serialize(param1), RpcParam[B].serialize(param2)))

    def call[A: RpcParam, B: RpcParam, C: RpcParam](methodName: String, param1: A, param2: B, param3: C): Task[Json] =
      call(methodName, List(RpcParam[A].serialize(param1), RpcParam[B].serialize(param2), RpcParam[C].serialize(param3)))

    def call(methodName: String, params: List[String]): Task[Json] = {
      val req = buildRequest(mantisUrl, methodName, params)

      Task.deferFuture(http.singleRequest(req)).flatMap(parseResponse)
    }
  }

  def parseResponse(res: HttpResponse)(implicit materializer: ActorMaterializer): Task[Json] = {
    Task.deferFuture(Unmarshal(res.entity).to[String])
      .tap(res => println(s"Unmarshalled: $res"))
      .map(parse(_).getOrElse(Json.Null))
  }

  def buildRequest(mantisUrl: String, methodName: String, params: List[String]): HttpRequest = {
    val payload = s"""{
                     |"jsonrpc":"2.0",
                     |"method":"$methodName",
                     |"params": ${params.mkString("[", ", ", "]")},
                     |"id": 1
                     |}""".stripMargin

    println(s"Calling RPC with payload: $payload")

    HttpRequest(
      method = HttpMethods.POST,
      uri = mantisUrl,
      entity = HttpEntity(ContentTypes.`application/json`, payload))
  }
}

trait RpcParam[T] {
  def serialize(param: T): String
}

object RpcParam {
  implicit val stringRpcParam: RpcParam[String] = param => s""""$param""""
  implicit val intRpcParam: RpcParam[Int] = param => param.toString
  implicit val bigIntRpcParam: RpcParam[BigInt] = param => s""""0x${param.toString(16)}""""
  implicit val bigIntegerRpcParam: RpcParam[BigInteger] = param => s""""0x${param.toString(16)}""""
  implicit val nullRpcParam: RpcParam[Null] = _ => "null"

  def apply[T](implicit instance: RpcParam[T]): RpcParam[T] = instance
}

trait SigningSupport {
  val credentials = Credentials.create(keyForAccount)

  val addEthHeader: String => String = s => "\u0019Ethereum Signed Message:\n" + s.length + s
  val toBytes: String => Array[Byte] = s => s.toCharArray.map(_.toByte)
  val toHex: Array[Byte] => String = Hex.toHexString
  val stringifySignature: SignatureData => String =
    signed => List(signed.getR, signed.getS, Array(signed.getV)).map(toHex).mkString("0x", "", "")
  // simplified implementation of https://github.com/ethereum/wiki/wiki/JSON-RPC#eth_sign
  val signStr: String => String = addEthHeader andThen toBytes andThen (Sign
    .signMessage(_, credentials.getEcKeyPair)) andThen stringifySignature
  val prepareForSign: String => String = toBytes andThen toHex
}
