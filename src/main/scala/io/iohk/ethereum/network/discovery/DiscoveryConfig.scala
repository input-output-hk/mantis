package io.iohk.ethereum.network.discovery

case class DiscoveryConfig(interface: String, port: Int, bootstrapNodes: Set[String])

object DiscoveryConfig {
  def apply(etcClientConfig: com.typesafe.config.Config): DiscoveryConfig = {
    import scala.collection.JavaConverters._
    val discoveryConfig = etcClientConfig.getConfig("network.discovery")
    val bootstrapNodes = discoveryConfig.getStringList("bootstrap-nodes").asScala.toSet
    DiscoveryConfig(discoveryConfig.getString("interface"), discoveryConfig.getInt("port"), bootstrapNodes)
  }
}
