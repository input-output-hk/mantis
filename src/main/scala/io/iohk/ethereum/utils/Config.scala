package io.iohk.ethereum.utils

import com.typesafe.config.ConfigFactory
import scala.collection.JavaConversions._
import scala.concurrent.duration._

object Config {

  private val config = ConfigFactory.load().getConfig("etc-client")

  object Server {
    private val serverConfig = config.getConfig("server-address")

    val interface = serverConfig.getString("interface")
    val port = serverConfig.getInt("port")
  }

  object Discovery {
    private val discoveryConfig = config.getConfig("discovery")

    val bootstrapNodes = discoveryConfig.getStringList("bootstrap-nodes").toList
  }

  object Peer {
    private val peerConfig = config.getConfig("peer")

    val connectRetryDelay = peerConfig.getDuration("connect-retry-delay").toMillis.millis
    val connectMaxRetries = peerConfig.getInt("connect-max-retries")
  }

}
