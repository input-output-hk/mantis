package io.iohk.ethereum.consensus
package atomixraft

import java.io.File

import com.typesafe.config.{Config ⇒ TypesafeConfig}
import io.atomix.messaging.impl.NettyMessagingService
import io.atomix.cluster.{Node ⇒ AtomixNode, NodeId ⇒ AtomixNodeId}
import io.atomix.messaging.{Endpoint ⇒ AtomixEndpoint}
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.Logger

case class AtomixRaftConfig private(
  coinbase: Address,
  localNode: AtomixNode,
  bootstrapNodes: List[AtomixNode],
  dataDir: File
)

object AtomixRaftConfig extends Logger {

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
    val coinbase = Address(config.getString("coinbase"))
    val localNode = parseNode(config.getString("local-node"))
    // In configuration, we can specify all nodes as bootstrap nodes, for convenience
    val bootstrapNodes_ = config.getStringList("bootstrap-nodes").asScala.map(parseNode).toList
    // In reality, the API requires all the _other_ nodes, so we just remove ourselves
    val bootstrapNodes = bootstrapNodes_.filterNot(_.id() == localNode.id())
    val dataDir = new File(config.getString("data-dir"))

    log.info("***** local-node = " + localNode)
    log.info("***** bootstrap-nodes = " + bootstrapNodes)

    new AtomixRaftConfig(
      coinbase = coinbase,
      localNode = localNode,
      bootstrapNodes = bootstrapNodes,
      dataDir = dataDir
    )
  }
}
