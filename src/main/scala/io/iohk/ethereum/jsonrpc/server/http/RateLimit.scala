package io.iohk.ethereum.jsonrpc.server.http

import java.time.Clock

import akka.http.scaladsl.model.RemoteAddress
import com.twitter.util.LruMap
import io.iohk.ethereum.jsonrpc.server.http.JsonRpcHttpServer.JsonRpcHttpServerConfig

trait RateLimit {

  val config: JsonRpcHttpServerConfig

  val latestRequestTimestamps = new LruMap[RemoteAddress, Long](config.rateLimit.latestTimestampCacheSize)

  val clock: Clock = Clock.systemUTC()

  def isBelowRateLimit(clientAddress: RemoteAddress): Boolean = {
    val timeMillis = clock.instant().toEpochMilli
    val latestRequestTimestamp = latestRequestTimestamps.getOrElse(clientAddress, 0L)

    val response = latestRequestTimestamp + config.rateLimit.minRequestInterval.toMillis < timeMillis
    if (response) latestRequestTimestamps.put(clientAddress, timeMillis)
    response
  }
}
