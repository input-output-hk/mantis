package io.iohk.ethereum.jsonrpc.server.http

import java.net.InetAddress
import java.time.{Clock, Instant, ZoneId}
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{HttpOrigin, Origin}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.{JsonRpcHttpServerConfig, RateLimitConfig}
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcHealthChecker, JsonRpcResponse}
import monix.eval.Task
import org.json4s.JsonAST.{JInt, JString}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import akka.http.scaladsl.model.headers._
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController

import scala.concurrent.duration.FiniteDuration

class JsonRpcHttpServerSpec extends AnyFlatSpec with Matchers with ScalatestRouteTest {

  it should "pass valid json request to controller" in new TestSetup {
    (mockJsonRpcController.handleRequest _)
      .expects(*)
      .returning(Task.now(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "asd", "id": "1"}""")
    val postRequest =
      HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~> Route.seal(mockJsonRpcHttpServer.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
  }

  it should "pass valid batch json request to controller" in new TestSetup {
    (mockJsonRpcController.handleRequest _)
      .expects(*)
      .twice()
      .returning(Task.now(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest =
      ByteString("""[{"jsonrpc":"2.0", "method": "asd", "id": "1"}, {"jsonrpc":"2.0", "method": "asd", "id": "2"}]""")
    val postRequest =
      HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~> Route.seal(mockJsonRpcHttpServer.route) ~> check {
      status === StatusCodes.OK
      responseAs[String] shouldEqual """[{"jsonrpc":"2.0","result":"this is a response","id":1},{"jsonrpc":"2.0","result":"this is a response","id":1}]"""
    }
  }

  it should "return BadRequest when malformed request is received" in new TestSetup {
    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "this is not a valid json""")
    val postRequest =
      HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~> Route.seal(mockJsonRpcHttpServer.route) ~> check {
      status shouldEqual StatusCodes.BadRequest
    }
  }

  it should "return a CORS Error" in new TestSetup {
    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "eth_blockNumber", "id": "1"}""")
    val postRequest = HttpRequest(
      HttpMethods.POST,
      uri = "/",
      headers = Origin(HttpOrigin("http://non_accepted_origin.com")) :: Nil,
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )

    import mockJsonRpcHttpServerWithCors.myRejectionHandler
    postRequest ~> Route.seal(mockJsonRpcHttpServerWithCors.route) ~> check {
      status shouldEqual StatusCodes.Forbidden
    }
  }

  it should "accept CORS Requests" in new TestSetup {

    (mockJsonRpcController.handleRequest _)
      .expects(*)
      .returning(Task.now(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "eth_blockNumber", "id": "1"}""")
    val postRequest = HttpRequest(
      HttpMethods.POST,
      uri = "/",
      headers = Origin(corsAllowedOrigin) :: Nil,
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithCors.route) ~> check {
      status shouldEqual StatusCodes.OK
    }
  }

  it should "accept json request with ip restriction and only one request" in new TestSetup {
    (mockJsonRpcController.handleRequest _)
      .expects(*)
      .returning(Task.now(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "asd", "id": "1"}""")
    val postRequest =
      HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithRateLimit.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
  }

  it should "return too many requests error with ip-restriction enabled and two requests executed in a row" in new TestSetup {
    (mockJsonRpcController.handleRequest _)
      .expects(*)
      .returning(Task.now(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "asd", "id": "1"}""")
    val postRequest =
      HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithRateLimit.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
    postRequest ~> Route.seal(mockJsonRpcHttpServerWithRateLimit.route) ~> check {
      status shouldEqual StatusCodes.TooManyRequests
    }
  }

  it should "return method not allowed error for batch request with ip-restriction enabled" in new TestSetup {
    (mockJsonRpcController.handleRequest _)
      .expects(*)
      .twice()
      .returning(Task.now(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest =
      ByteString("""[{"jsonrpc":"2.0", "method": "asd", "id": "1"}, {"jsonrpc":"2.0", "method": "asd", "id": "2"}]""")
    val postRequest =
      HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithRateLimit.route) ~> check {
      status === StatusCodes.MethodNotAllowed
    }
  }

  it should "accept json request after rejected request with ip-restriction enabled once time has passed" in new TestSetup {
    (mockJsonRpcController.handleRequest _)
      .expects(*)
      .twice()
      .returning(Task.now(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "asd", "id": "1"}""")
    val postRequest =
      HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithRateLimit.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
    postRequest ~> Route.seal(mockJsonRpcHttpServerWithRateLimit.route) ~> check {
      status shouldEqual StatusCodes.TooManyRequests
    }

    fakeClock.advanceTime(2 * serverConfigWithRateLimit.rateLimit.minRequestInterval.toMillis)

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithRateLimit.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
  }

  it should "accept json requests from different IPs with ip-restriction enabled" in new TestSetup {
    (mockJsonRpcController.handleRequest _)
      .expects(*)
      .twice()
      .returning(Task.now(JsonRpcResponse("2.0", Some(JString("this is a response")), None, JInt(1))))

    val jsonRequest = ByteString("""{"jsonrpc":"2.0", "method": "asd", "id": "1"}""")
    val postRequest =
      HttpRequest(HttpMethods.POST, uri = "/", entity = HttpEntity(MediaTypes.`application/json`, jsonRequest))

    val postRequest2 =
      HttpRequest(
        HttpMethods.POST,
        uri = "/",
        headers = List(`X-Forwarded-For`(RemoteAddress.apply(InetAddress.getByName("1.2.3.4")))),
        entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
      )

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithRateLimit.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
    postRequest2 ~> Route.seal(mockJsonRpcHttpServerWithRateLimit.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
  }

  trait TestSetup extends MockFactory {
    val rateLimitConfig = new RateLimitConfig {
      override val enabled: Boolean = false
      override val minRequestInterval: FiniteDuration = FiniteDuration.apply(5, TimeUnit.SECONDS)
      override val latestTimestampCacheSize: Int = 1024
    }

    val serverConfig = new JsonRpcHttpServerConfig {
      override val mode: String = "mockJsonRpc"
      override val enabled: Boolean = true
      override val interface: String = ""
      override val port: Int = 123
      override val corsAllowedOrigins = HttpOriginMatcher.*
      override val rateLimit: RateLimitConfig = rateLimitConfig
    }

    val rateLimitEnabledConfig = new RateLimitConfig {
      override val enabled: Boolean = true
      override val minRequestInterval: FiniteDuration = FiniteDuration.apply(5, TimeUnit.SECONDS)
      override val latestTimestampCacheSize: Int = 1024
    }

    val serverConfigWithRateLimit = new JsonRpcHttpServerConfig {
      override val mode: String = "mockJsonRpc"
      override val enabled: Boolean = true
      override val interface: String = ""
      override val port: Int = 123
      override val corsAllowedOrigins = HttpOriginMatcher.*
      override val rateLimit: RateLimitConfig = rateLimitEnabledConfig
    }

    val mockJsonRpcController = mock[JsonRpcController]
    val mockJsonRpcHealthChecker = mock[JsonRpcHealthChecker]
    val fakeClock = new FakeClock

    val mockJsonRpcHttpServer = new FakeJsonRpcHttpServer(
      jsonRpcController = mockJsonRpcController,
      jsonRpcHealthChecker = mockJsonRpcHealthChecker,
      config = serverConfig,
      cors = serverConfig.corsAllowedOrigins,
      testClock = fakeClock
    )

    val corsAllowedOrigin = HttpOrigin("http://localhost:3333")
    val mockJsonRpcHttpServerWithCors = new FakeJsonRpcHttpServer(
      jsonRpcController = mockJsonRpcController,
      jsonRpcHealthChecker = mockJsonRpcHealthChecker,
      config = serverConfig,
      cors = HttpOriginMatcher(corsAllowedOrigin),
      testClock = fakeClock
    )

    val mockJsonRpcHttpServerWithRateLimit = new FakeJsonRpcHttpServer(
      jsonRpcController = mockJsonRpcController,
      jsonRpcHealthChecker = mockJsonRpcHealthChecker,
      config = serverConfigWithRateLimit,
      cors = serverConfigWithRateLimit.corsAllowedOrigins,
      testClock = fakeClock
    )
  }
}

class FakeJsonRpcHttpServer(
    val jsonRpcController: JsonRpcBaseController,
    val jsonRpcHealthChecker: JsonRpcHealthChecker,
    val config: JsonRpcHttpServerConfig,
    val cors: HttpOriginMatcher,
    val testClock: Clock
)(implicit val actorSystem: ActorSystem)
    extends JsonRpcHttpServer
    with Logger {
  def run(): Unit = ()
  override def corsAllowedOrigins: HttpOriginMatcher = cors
  override val clock = testClock
}

class FakeClock extends Clock {

  var time: Instant = Instant.now()

  def advanceTime(seconds: Long): Unit = {
    time = time.plusSeconds(seconds)
  }

  override def getZone: ZoneId = ZoneId.systemDefault()

  override def withZone(zone: ZoneId): Clock = Clock.fixed(time, getZone)

  override def instant(): Instant = time
}
