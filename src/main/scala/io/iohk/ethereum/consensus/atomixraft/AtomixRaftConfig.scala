package io.iohk.ethereum.consensus
package atomixraft

import java.io.File

import com.typesafe.config.{Config ⇒ TypesafeConfig}
import io.atomix.cluster.{Member ⇒ AtomixMember, MemberId ⇒ AtomixMemberId}
import io.atomix.utils.net.{Address ⇒ AtomixAddress}
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.{FiniteDuration, _}

case class AtomixRaftConfig private(
  localNode: AtomixMember,
  bootstrapNodes: List[AtomixMember],
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

  def parseMemberId(parts: Array[String]): AtomixMemberId =
    parts.length match {
      case 1 | 3 ⇒ AtomixMemberId.from(parts(0).trim)
      case 2     ⇒ AtomixMemberId.from(parts(0).trim + "_" + parts(1).trim)
      case _     ⇒ throw new IllegalArgumentException("parts.length != 1, 2, 3")
    }

  def parseAddress(parts: Array[String]): AtomixAddress =
    parts.length match {
      case 1 ⇒ AtomixAddress.from(parts(0).trim, AtomixAddress.local().port()) // Default port: 5679
      case 2 ⇒ AtomixAddress.from(parts(0).trim, parts(1).trim.toInt)
      case 3 ⇒ AtomixAddress.from(parts(1).trim, parts(2).trim.toInt)
      case _ ⇒ throw new IllegalArgumentException("parts.length != 1, 2, 3")
    }

  def parseMember(id_host_port: String): AtomixMember = {
    val parts = id_host_port.split(":")
    val id = parseMemberId(parts)
    val address = parseAddress(parts)

    // FIXME Where is type in the new API? (// https://groups.google.com/forum/#!topic/atomixio/iv2MYbBoB_M)
    AtomixMember.builder
      .withId(id)
      .withAddress(address)
      .build
  }

  def apply(mantisConfig: TypesafeConfig): AtomixRaftConfig = {
    import scala.collection.JavaConverters._

    val config = mantisConfig.getConfig(Protocol.Names.AtomixRaft)
    val localNode = parseMember(config.getString(Keys.LocalNode))

    // In configuration, we can specify all nodes as bootstrap nodes, for convenience
    val bootstrapNodes_ = config.getStringList(Keys.BootstrapNodes).asScala.map(parseMember).toList
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
