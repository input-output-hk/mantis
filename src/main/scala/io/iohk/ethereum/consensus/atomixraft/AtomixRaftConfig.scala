package io.iohk.ethereum.consensus
package atomixraft

import java.io.File

import com.typesafe.config.{Config ⇒ TypesafeConfig}
import io.atomix.cluster.{Node ⇒ AtomixNode, NodeId ⇒ AtomixNodeId}
import io.atomix.messaging.impl.NettyMessagingService
import io.atomix.messaging.{Endpoint ⇒ AtomixEndpoint}
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.{FiniteDuration, _}

case class AtomixRaftConfig private(
  localNode: AtomixNode,
  bootstrapNodes: List[AtomixNode],
  dataDir: File,
  electionTimeout: FiniteDuration,
  heartbeatInterval: FiniteDuration,
  blockForgingDelay: FiniteDuration
)

object AtomixRaftConfig extends Logger {
  object Keys {
    final val LocalNode = "local-node"
    final val BootstrapNodes = "bootstrap-nodes"
    final val DataDir = "data-dir"
    final val ElectionTimeout = "election-timeout"
    final val HeartbeatInterval = "heartbeat-interval"
    final val BlockForgingDelay = "block-forging-delay"
  }

  def parseNodeId(parts: Array[String]): AtomixNodeId =
    parts.length match {
      case 1 | 3 ⇒ AtomixNodeId.from(parts(0).trim)
      case 2     ⇒ AtomixNodeId.from(parts(0).trim + "_" + parts(1).trim)
      case _     ⇒ throw new IllegalArgumentException("parts.length != 1, 2, 3")
    }

  def parseEndpoint(parts: Array[String]): AtomixEndpoint =
    parts.length match {
      case 1 ⇒ AtomixEndpoint.from(parts(0).trim, NettyMessagingService.DEFAULT_PORT)
      case 2 ⇒ AtomixEndpoint.from(parts(0).trim, parts(1).trim.toInt)
      case 3 ⇒ AtomixEndpoint.from(parts(1).trim, parts(2).trim.toInt)
      case _ ⇒ throw new IllegalArgumentException("parts.length != 1, 2, 3")
    }

  def parseNode(id_host_port: String): AtomixNode = {
    val parts = id_host_port.split(":")
    val nodeId = parseNodeId(parts)
    val endpoint = parseEndpoint(parts)

    AtomixNode.builder
      .withId(nodeId)
      .withEndpoint(endpoint)
      .withType(AtomixNode.Type.DATA)
      .build
  }

  def apply(mantisConfig: TypesafeConfig): AtomixRaftConfig = {
    import scala.collection.JavaConverters._

    val config = mantisConfig.getConfig(Protocol.Names.AtomixRaft)
    val localNode = parseNode(config.getString(Keys.LocalNode))

    // In configuration, we can specify all nodes as bootstrap nodes, for convenience
    val bootstrapNodes_ = config.getStringList(Keys.BootstrapNodes).asScala.map(parseNode).toList
    // In reality, the API requires all the _other_ nodes, so we just remove ourselves
    val bootstrapNodes = bootstrapNodes_.filterNot(_.id() == localNode.id())
    val dataDir = new File(config.getString(Keys.DataDir))
    val electionTimeout = config.getDuration(Keys.ElectionTimeout).toMillis.millis
    val heartbeatInterval = config.getDuration(Keys.HeartbeatInterval).toMillis.millis
    val blockForgingDelay = config.getDuration(Keys.BlockForgingDelay).toMillis.millis

    log.info("***** local-node = " + localNode)
    log.info("***** bootstrap-nodes = " + bootstrapNodes)

    new AtomixRaftConfig(
      localNode = localNode,
      bootstrapNodes = bootstrapNodes,
      dataDir = dataDir,
      electionTimeout = electionTimeout,
      heartbeatInterval = heartbeatInterval,
      blockForgingDelay = blockForgingDelay
    )
  }
}
