package io.iohk.ethereum.network.discovery

import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.util.{Failure, Success}

case class DiscoveryConfig(
    discoveryEnabled: Boolean,
    interface: String,
    port: Int,
    bootstrapNodes: Set[Node],
    nodesLimit: Int /* TODO: remove once proper discovery protocol is in place */,
    scanMaxNodes: Int /* TODO: remove once proper discovery protocol is in place */,
    scanInitialDelay: FiniteDuration,
    scanInterval: FiniteDuration,
    messageExpiration: FiniteDuration)

object DiscoveryConfig extends Logger {
  def apply(etcClientConfig: com.typesafe.config.Config): DiscoveryConfig = {
    import scala.collection.JavaConverters._
    val discoveryConfig = etcClientConfig.getConfig("network.discovery")
    val bootstrapNodes = parseBootstrapNodes(discoveryConfig.getStringList("bootstrap-nodes").asScala.toSet)

    DiscoveryConfig(
      discoveryEnabled = discoveryConfig.getBoolean("discovery-enabled"),
      interface = discoveryConfig.getString("interface"),
      port = discoveryConfig.getInt("port"),
      bootstrapNodes = bootstrapNodes,
      nodesLimit = discoveryConfig.getInt("nodes-limit"),
      scanMaxNodes = discoveryConfig.getInt("scan-max-nodes"),
      scanInitialDelay = discoveryConfig.getDuration("scan-initial-delay").toMillis.millis,
      scanInterval = discoveryConfig.getDuration("scan-interval").toMillis.millis,
      messageExpiration = discoveryConfig.getDuration("message-expiration").toMillis.millis)
  }

  /**
    * Parses all the bootstrap nodes, logging the invalid ones and returning the valid ones
    *
    * @param unParsedBootStrapNodes, with the bootstrap nodes to be parsed
    * @return set of parsed and valid bootstrap nodes
    */
  private def parseBootstrapNodes(unParsedBootStrapNodes: Set[String]): Set[Node] = unParsedBootStrapNodes.foldLeft[Set[Node]](Set.empty) {
    case (parsedBootstrapNodes, nodeString) =>
      val maybeNode = NodeParser.parseNode(nodeString)
      maybeNode match {
        case Success(node) => parsedBootstrapNodes + node
        case Failure(error) =>
          log.warn(s"Unable to parse node: $nodeString due to: ${error.getMessage}")
          parsedBootstrapNodes
      }
  }
}
