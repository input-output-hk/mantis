package io.iohk.ethereum.rpcTest.utils

import java.io.File
import java.nio.file._

import cats.effect.Resource
import com.typesafe.config.{Config, ConfigFactory}
import io.iohk.ethereum.rpcTest.RpcTestConfig
import io.iohk.ethereum.rpcTest.utils.MantisProcessBuilder._
import io.iohk.ethereum.utils.FunctorOps._
import monix.eval.Task
import monix.reactive.subjects.{PublishSubject, ReplaySubject}
import mouse.all._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.sys.process.{Process, ProcessLogger, _}

class MantisProcessBuilder(network: Option[String] = None, params: Params = Map()) {
  val datadirKey = "mantis.datadir"
  val miningKey = "mantis.consensus.mining-enabled"

  //TODO: make this env/config
  val patchedMantisBuild = new File(
    "/Users/andrzej_kopec/scalac/iohk/mantis/target/universal/mantis-rpc-tests/mantis-1.1-rc1")

  def withTemporaryDatadir(): MantisProcessBuilder = withTemporaryDatadir(getTempDatadir())
  def withTemporaryDatadir(dir: String): MantisProcessBuilder = withParam(datadirKey, dir)

  def withMiningEnabled(): MantisProcessBuilder = withParam(miningKey, true)

  def withMiningDisabled(): MantisProcessBuilder = withParam(miningKey, false)

  def withDiscoveryEnabled(): MantisProcessBuilder = withPeersCommunication().withParams(Map(
    "mantis.network.discovery.discovery-enabled" -> true,
    "mantis.network.discovery.port" -> 30304
  ))

  def withPeersCommunication(): MantisProcessBuilder = withParams(Map(
    "mantis.network.peer.max-incoming-peers" -> 10,
    "mantis.network.peer.max-pending-peers" -> 80,
    "mantis.network.server-address.port" -> 9078
  ))

  def withFastSyncEnabled(): MantisProcessBuilder = withParam("mantis.sync.do-fast-sync", true)

  def withNetwork(network: String): MantisProcessBuilder = copy(network = Some(network))

  def withParam(key: String, value: Any): MantisProcessBuilder = copy(params = params + (key -> value))
  def withParams(newParams: Params): MantisProcessBuilder = copy(params = params ++ newParams)

  def run[T](cb: MantisProcess => Task[T])(implicit testConfig: RpcTestConfig): Task[T] = create().use(cb)
  def create()(implicit testConfig: RpcTestConfig): Resource[Task, MantisProcess] = Resource.make {

    val paramsWithNetwork = network.fold(params)(net => params + ("mantis.blockchains.network" -> net))

    println(s"params: ${Seq("./bin/mantis") ++ paramsToProperties(paramsWithNetwork)}")
    val underlyingProcessBuilder = Process(Seq("./bin/mantis") ++ paramsToProperties(paramsWithNetwork), patchedMantisBuild)
    val linesFromBeginning = ReplaySubject[String]()
    val lines = PublishSubject[String]()
    val logger = ProcessLogger(line => {
      println(line)
      lines.onNext(line)
      linesFromBeginning.onNext(line)
    })

    ensureKeystoreCopy(params)
      .flatMap(
        _ =>
          Task
            .delay(underlyingProcessBuilder.run(logger))
            .map(new MantisProcess(_, lines, linesFromBeginning)))
  } { mantisProc => ProcessUtils.kill(mantisProc.underlyingProcess) }

  def run(processes: mutable.Buffer[Process], network: String, config: Params = Map()): Task[MantisProcess] =
    run(processes, network, ConfigFactory.parseMap((params ++ config).asJava))

  def run(processes: mutable.Buffer[Process], network: String, config: Config): Task[MantisProcess] = {
    println(s"params: ${Seq("./bin/mantis", s"-Dmantis.blockchains.network=$network") ++ configToProperties(config)}")

    val underlyingProcessBuilder =
      Process(
        Seq("./bin/mantis", s"-Dmantis.blockchains.network=$network") ++ configToProperties(config),
        patchedMantisBuild)
    val linesFromBeginning = ReplaySubject[String]()
    val lines = PublishSubject[String]()
    val logger = ProcessLogger(line => {
      println(line)
      lines.onNext(line)
      linesFromBeginning.onNext(line)
    })

    ensureKeystoreCopy(params)
      .flatMap(
        _ =>
          Task
            .delay(underlyingProcessBuilder.run(logger))
            .tap(processes += _)
            .map(new MantisProcess(_, lines, linesFromBeginning)))
  }

  private def ensureKeystoreCopy(params: Params): Task[Unit] =
    params
      .get(datadirKey)
      .map(path => path |> (_.asInstanceOf[String]) |> (new File(_)) |> copyKeystore)
      .getOrElse(Task.pure(()))

  private def copyKeystore(datadir: File): Task[Unit] =
    Task {
      val originalKeystore = new File(getClass.getResource("/privateNetConfig/keystore").getPath).toPath
      val dataDirKeystore = datadir.toPath.resolve("keystore")

      s"cp -R $originalKeystore/ $dataDirKeystore/".! |> { res => assert(res == 0, "Copying keystore failed") }
    }

  private def copy(network: Option[String] = network, params: Params = params): MantisProcessBuilder = MantisProcessBuilder(network, params)
}

object MantisProcessBuilder {
  type Params = Map[String, Any]

  def apply(network: Option[String] = None, params: Params = Map()): MantisProcessBuilder = new MantisProcessBuilder(network, params)

  //TODO: Wrap this in task
  def getTempDatadir(): String = Files.createTempDirectory("mantis-rpc-test").toAbsolutePath.toString

  private def configToProperties(config: Config): Seq[String] =
    config
      .entrySet()
      .asScala
      .map(entry => s"-D${entry.getKey}=${entry.getValue.unwrapped()}")
      .toSeq

  private def paramsToProperties(params: Params): Seq[String] =
    params.toList.map {
      case (key, value) => s"-D$key=$value"
    }
}
