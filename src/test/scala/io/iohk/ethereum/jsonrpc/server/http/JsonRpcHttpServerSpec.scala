package io.iohk.ethereum.jsonrpc.server.http

import java.time.{Clock, Instant, ZoneId}
import java.util.concurrent.TimeUnit

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{HttpOrigin, Origin}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import com.twitter.util.LruMap
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcHealthChecker, JsonRpcResponse}
import monix.eval.Task
import org.json4s.JsonAST.{JInt, JString}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithIpRestriction.route) ~> check {
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

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithIpRestriction.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
    postRequest ~> Route.seal(mockJsonRpcHttpServerWithIpRestriction.route) ~> check {
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

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithIpRestriction.route) ~> check {
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

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithIpRestriction.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
    postRequest ~> Route.seal(mockJsonRpcHttpServerWithIpRestriction.route) ~> check {
      status shouldEqual StatusCodes.TooManyRequests
    }

    FakeClock.advanceTime(10)

    postRequest ~> Route.seal(mockJsonRpcHttpServerWithIpRestriction.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual """{"jsonrpc":"2.0","result":"this is a response","id":1}"""
    }
  }

  trait TestSetup extends MockFactory {
    val config = new JsonRpcHttpServerConfig {
      override val mode: String = "mockJsonRpc"
      override val enabled: Boolean = true
      override val interface: String = ""
      override val port: Int = 123
      override val certificateKeyStorePath = None
      override val certificateKeyStoreType = None
      override val certificatePasswordFile = None
      override val corsAllowedOrigins = HttpOriginMatcher.*
      override val ipTrackingEnabled: Boolean = false
      override val minRequestInterval: FiniteDuration = FiniteDuration.apply(5, TimeUnit.SECONDS)
      override val latestTimestampCacheSize: Int = 1024
    }

    val mockJsonRpcController = mock[JsonRpcController]
    val mockJsonRpcHealthChecker = mock[JsonRpcHealthChecker]
    val mockJsonRpcHttpServer = new JsonRpcHttpServer {
      override val jsonRpcController = mockJsonRpcController
      override val jsonRpcHealthChecker = mockJsonRpcHealthChecker

      override val ipTrackingEnabled: Boolean = config.ipTrackingEnabled
      override val minRequestInterval: FiniteDuration = config.minRequestInterval
      override val latestTimestampCacheSize: Int = config.latestTimestampCacheSize
      override val latestRequestTimestamps = new LruMap[RemoteAddress, Long](latestTimestampCacheSize)

      def run(): Unit = ()

      override def corsAllowedOrigins: HttpOriginMatcher = config.corsAllowedOrigins
    }

    val corsAllowedOrigin = HttpOrigin("http://localhost:3333")

    val mockJsonRpcHttpServerWithCors = new JsonRpcHttpServer {
      override val jsonRpcController = mockJsonRpcController
      override val jsonRpcHealthChecker = mockJsonRpcHealthChecker

      override val ipTrackingEnabled: Boolean = config.ipTrackingEnabled
      override val minRequestInterval: FiniteDuration = config.minRequestInterval
      override val latestTimestampCacheSize: Int = config.latestTimestampCacheSize
      override val latestRequestTimestamps = new LruMap[RemoteAddress, Long](latestTimestampCacheSize)

      def run(): Unit = ()

      override def corsAllowedOrigins: HttpOriginMatcher = HttpOriginMatcher(corsAllowedOrigin)
    }

    val mockJsonRpcHttpServerWithIpRestriction = new JsonRpcHttpServer {
      override val jsonRpcController = mockJsonRpcController
      override val jsonRpcHealthChecker = mockJsonRpcHealthChecker

      override val clock: Clock = FakeClock

      override val ipTrackingEnabled: Boolean = true
      override val minRequestInterval: FiniteDuration = config.minRequestInterval
      override val latestTimestampCacheSize: Int = config.latestTimestampCacheSize
      override val latestRequestTimestamps = new LruMap[RemoteAddress, Long](latestTimestampCacheSize)

      def run(): Unit = ()

      override def corsAllowedOrigins: HttpOriginMatcher = config.corsAllowedOrigins
    }
  }
}

object FakeClock extends Clock {

  var time: Instant = Instant.now()

  def advanceTime(seconds: Long): Unit = {
    time = time.plusSeconds(seconds)
  }

  override def getZone: ZoneId = ZoneId.systemDefault()

  override def withZone(zone: ZoneId): Clock = Clock.fixed(time, getZone)

  override def instant(): Instant = time
}