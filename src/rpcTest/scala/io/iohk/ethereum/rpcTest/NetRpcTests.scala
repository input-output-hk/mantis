package io.iohk.ethereum.rpcTest
import java.nio.file.Files

import cats.Traverse
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.rpcTest.data.Tags._
import monix.execution.Scheduler
import org.scalatest.{BeforeAndAfterEach, FreeSpec, Matchers}

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.sys.process._
import scala.collection.JavaConverters._
import cats.instances.list._
import io.iohk.ethereum.rpcTest.utils.MantisProcess
import monix.eval.Task
import monix.reactive.Observable

class NetRpcTests extends FreeSpec with Matchers with BeforeAndAfterEach {
  implicit val scheduler: Scheduler = monix.execution.Scheduler(scala.concurrent.ExecutionContext.global)
  val defaultTimeout: FiniteDuration = 30.seconds
  val defaultNetwork = "rpc-test-private"

  var processes: mutable.Buffer[Process] = _

  override def beforeEach(): Unit =
    processes = mutable.ArrayBuffer()

  override def afterEach(): Unit =
    processes.foreach(_.destroy())

  val allowPeersSettings = Map(
    "mantis.network.peer.max-incoming-peers" -> 4,
    "mantis.network.peer.max-pending-peers" -> 4,
  )

  "net_version" - {
    "returns 1 for main net" taggedAs (MainNet, Standalone, Net) in new ScenarioSetup {
      val proc = MantisProcess.run(processes, "etc").flatMap(_.waitForRpc())
      Await.result(proc.runToFuture, defaultTimeout)

      service.netVersion().send().getNetVersion shouldBe "1"
    }

    "returns 2 for morden" taggedAs (Morden, Standalone, Net) in new ScenarioSetup {
      val proc = MantisProcess.run(processes, "morden").flatMap(_.waitForRpc())
      Await.result(proc.runToFuture, defaultTimeout)

      service.netVersion().send().getNetVersion shouldBe "2"
    }

    "returns 3 for ropsten" taggedAs (Ropsten, Standalone, Net) in new ScenarioSetup {
      val proc = MantisProcess.run(processes, "ropsten").flatMap(_.waitForRpc())
      Await.result(proc.runToFuture, defaultTimeout)

      service.netVersion().send().getNetVersion shouldBe "3"
    }
  }

  "net_peerCount" - {
    "connected only to bootstrap nodes" taggedAs (Standalone, Net) in new ScenarioSetup {
      val bootstrapNode1 = MantisProcess.run(
        processes = processes,
        network = defaultNetwork,
        config = ConfigFactory.parseMap(
          (allowPeersSettings ++ Map(
            "mantis.network.server-address.port" -> 9078,
            "mantis.network.rpc.http.enabled" -> false,
            "mantis.datadir" -> Files.createTempDirectory("mantis-rpc-test").toAbsolutePath.toString
          )).asJava)
      )
      val bootstrapNode2 = MantisProcess.run(
        processes = processes,
        network = defaultNetwork,
        config = ConfigFactory.parseMap(
          (allowPeersSettings ++ Map(
            "mantis.network.server-address.port" -> 9079,
            "mantis.network.rpc.http.enabled" -> false,
            "mantis.datadir" -> Files.createTempDirectory("mantis-rpc-test").toAbsolutePath.toString
          )).asJava)
      )

      val result = Traverse[List]
        .sequence(bootstrapNode1 :: bootstrapNode2 :: Nil)
        .flatMap(Traverse[List].traverse(_)(_.nodeAddress()))
        .map(bootstrapNodes =>
          bootstrapNodes
            .zip(0 to bootstrapNodes.size)
            .map {
              case (node, index) => s"mantis.blockchains.$defaultNetwork.bootstrap-nodes.$index" -> node
            }
            .toMap)
        .flatMap(bootstrapNodes => {
          println("Running target client")
          MantisProcess.run(
            processes = processes,
            network = defaultNetwork,
            config = ConfigFactory.parseMap(
              (bootstrapNodes ++ allowPeersSettings + ("mantis.consensus.mining-enabled" -> false)).asJava)
          )
        })
        .flatMap(_.waitForRpc())
        .flatMap(
          _ =>
            Observable
              .interval(1.second)
              .map(_ => service.netPeerCount().send().getQuantity)
              .filter(_.compareTo(2) == 0)
              .firstL)
        .onErrorHandleWith(err => {
          println(s"Error $err")
          Task.raiseError(err)
        })

      Await.result(result.runToFuture, 3.minutes)
    }

    "connected and then disconnected" taggedAs (Standalone, Net) in new ScenarioSetup {
      val targetProc = MantisProcess.run(
        processes,
        defaultNetwork,
        allowPeersSettings + ("mantis.consensus.mining-enabled" -> false))

      val peerProc: MantisProcess => Task[MantisProcess] = proc =>
        proc
          .nodeAddress()
          .flatMap(address =>
            MantisProcess.run(
              processes,
              defaultNetwork,
              (allowPeersSettings ++ Map(
                s"mantis.blockchains.$defaultNetwork.bootstrap-nodes.0" -> address,
                "mantis.network.server-address.port" -> 9078,
                "mantis.network.rpc.http.enabled" -> false,
                "mantis.datadir" -> Files.createTempDirectory("mantis-rpc-test").toAbsolutePath.toString,
                "mantis.consensus.mining-enabled" -> false
              ))
          ))

      val result = targetProc
        .flatMap(proc => proc.waitForRpc().map(_ => proc))
        .flatMap(proc => {
          peerProc(proc).flatMap(peer => {
            Observable
              .interval(1.second)
              .map(_ => service.netPeerCount().send().getQuantity)
              .filter(_.compareTo(1) == 0)
              .firstL
              .flatMap(_ => Task { peer.underlyingProcess.destroy() })
              .flatMap(
                _ =>
                  Observable
                    .interval(1.second)
                    .map(_ => service.netPeerCount().send().getQuantity)
                    .filter(_.compareTo(0) == 0)
                    .firstL)
          })
        })

      Await.result(result.runToFuture, 1.minute)
    }
  }
}
