package io.iohk.ethereum.jsonrpc.server.http

import java.time.Duration

import akka.NotUsed
import akka.http.scaladsl.model.{RemoteAddress, StatusCodes}
import akka.http.scaladsl.server.{Directive0, Route}
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.RateLimitConfig
import akka.http.scaladsl.server.Directives._
import com.google.common.base.Ticker
import com.google.common.cache.CacheBuilder
import io.iohk.ethereum.jsonrpc.JsonRpcError
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers
import org.json4s.{DefaultFormats, Formats, Serialization, native}

class RateLimit(config: RateLimitConfig) extends Directive0 with Json4sSupport {

  private implicit val serialization: Serialization = native.Serialization
  private implicit val formats: Formats = DefaultFormats + JsonSerializers.RpcErrorJsonSerializer

  private[this] lazy val minInterval = config.minRequestInterval.toSeconds

  private[this] lazy val lru = {
    val nanoDuration = config.minRequestInterval.toNanos
    val javaDuration = Duration.ofNanos(nanoDuration)
    val ticker: Ticker = new Ticker {
      override def read(): Long = getCurrentTimeNanos
    }
    CacheBuilder
      .newBuilder()
      .weakKeys()
      .expireAfterAccess(javaDuration)
      .ticker(ticker)
      .build[RemoteAddress, NotUsed]()
  }

  private[this] def isBelowRateLimit(ip: RemoteAddress): Boolean = {
    var exists = true
    lru.get(
      ip,
      () => {
        exists = false
        NotUsed
      }
    )
    exists
  }

  // Override this to test
  protected def getCurrentTimeNanos: Long = System.nanoTime()

  // Such algebras prevent if-elseif-else boilerplate in the JsonRPCServer code
  // It is also guaranteed that:
  //   1) no IP address is extracted unless config.enabled is true
  //   2) no LRU is created unless config.enabled is true
  //   3) cache is accessed only once (using get)
  override def tapply(f: Unit => Route): Route =
    if (config.enabled) {
      extractClientIP { ip =>
        if (isBelowRateLimit(ip)) {
          val err = JsonRpcError.RateLimitError(minInterval)
          complete((StatusCodes.TooManyRequests, err))
        } else {
          f.apply(())
        }
      }
    } else f.apply(())

}
