package io.iohk.ethereum.jsonrpc.server.http

import akka.http.scaladsl.model.{RemoteAddress, StatusCodes}
import akka.http.scaladsl.server.{Directive0, Route}
import io.iohk.ethereum.db.cache.SimpleLRU
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.RateLimitConfig
import akka.http.scaladsl.server.Directives._
import io.iohk.ethereum.jsonrpc.JsonRpcError
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers
import org.json4s.{DefaultFormats, Formats, Serialization, native}


class RateLimit(config: RateLimitConfig) extends Directive0 with Json4sSupport {

  private implicit val serialization: Serialization = native.Serialization
  private implicit val formats: Formats = DefaultFormats + JsonSerializers.RpcErrorJsonSerializer

  // It determines whether a request needs to be blocked
  // Such algebras prevent if-elseif-else boilerplate in the JsonRPCServer code
  val blockingAlgebra: (RemoteAddress => Boolean) = {
    if (config.enabled) {
      val lru = new SimpleLRU[RemoteAddress](
        config.latestTimestampCacheSize,
        config.minRequestInterval.toMillis
      )
      lru.checkAndRefreshEntry
    } else {
      _ => false
    }
  }

  override def tapply(f: Unit => Route): Route = extractClientIP { ip =>
    if (blockingAlgebra(ip)) {
      val err = JsonRpcError.RateLimitError(config.minRequestInterval.toSeconds)
      complete((StatusCodes.TooManyRequests, err))
    } else {
      f.apply( () )
    }
  }

}