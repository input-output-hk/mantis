package io.iohk.ethereum.network

import java.net.URI

import akka.actor.{Actor, ActorLogging, Props, Scheduler}
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.ethereum.network.KnownNodesManager.KnownNodesManagerConfig

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class KnownNodesManager(
    config: KnownNodesManagerConfig,
    knownNodesStorage: KnownNodesStorage,
    externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor with ActorLogging {

  import KnownNodesManager._

  private def scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  var knownNodes: Set[URI] = knownNodesStorage.getKnownNodes()

  var toAdd: Set[URI] = Set.empty

  var toRemove: Set[URI] = Set.empty

  scheduler.schedule(config.persistInterval, config.persistInterval) {
    persistChanges()
  }

  override def receive: Receive = {
    case AddKnownNode(uri) =>
      if (!knownNodes.contains(uri)) {
        knownNodes += uri
        toAdd += uri
        toRemove -= uri
      }

    case RemoveKnownNode(uri) =>
      if (knownNodes.contains(uri)) {
        knownNodes -= uri
        toAdd -= uri
        toRemove += uri
      }

    case GetKnownNodes =>
      sender() ! KnownNodes(knownNodes)
  }

  private def persistChanges(): Unit = {
    log.debug(s"Persisting ${knownNodes.size} known nodes.")
    if (toAdd.nonEmpty || toRemove.nonEmpty) {
      knownNodesStorage.updateKnownNodes(
        toAdd = toAdd,
        toRemove = toRemove)
      toAdd = Set.empty
      toRemove = Set.empty
    }
  }

}

object KnownNodesManager {
  def props(config: KnownNodesManagerConfig, knownNodesStorage: KnownNodesStorage): Props =
    Props(new KnownNodesManager(config, knownNodesStorage))

  case class AddKnownNode(uri: URI)
  case class RemoveKnownNode(uri: URI)
  case object GetKnownNodes
  case class KnownNodes(nodes: Set[URI])

  case class KnownNodesManagerConfig(persistInterval: FiniteDuration)

  object KnownNodesManagerConfig {
    def apply(etcClientConfig: com.typesafe.config.Config): KnownNodesManagerConfig = {
      val knownNodesManagerConfig = etcClientConfig.getConfig("network.known-nodes")
      KnownNodesManagerConfig(
        persistInterval = knownNodesManagerConfig.getDuration("persist-interval").toMillis.millis)
    }
  }
}
