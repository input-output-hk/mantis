package io.iohk.ethereum.nodebuilder

import java.io.File
import java.lang.ProcessBuilder.Redirect
import java.net.URLClassLoader

import akka.actor.ActorSystem
import io.iohk.ethereum.extvm.{ExtVMInterface, VmServerApp}
import io.iohk.ethereum.utils.{BlockchainConfig, VmConfig}
import io.iohk.ethereum.utils.VmConfig.ExternalConfig
import io.iohk.ethereum.vm.VM

object VmSetup {

  import VmConfig.VmMode._
  import VmConfig.ExternalConfig.StartVmConfig._

  private def startMantisVmProcess(classpath: Array[String]): Unit = {
    import File.{separator, pathSeparator}

    new ProcessBuilder(
      System.getProperty("java.home") + "/bin/java",
      "-classpath",
      classpath.mkString(pathSeparator),
      "-Dconfig.file=." + separator + "conf" + separator + "mantis.conf",
      "-Dlogback.configurationFile=." + separator + "conf" + separator + "logback.xml",
      "io.iohk.ethereum.extvm.VmServerApp")
      .inheritIO()
      .start()
  }

  // for running from within SBT
  private def startMantisVmInThisProcess(): Unit = {
    VmServerApp.main(Array())
  }

  private def startIeleVmProcess(host: String, port: Int): Unit = {
    // TODO: we also need host parameter in iele node
    new ProcessBuilder(
      "bin/iele-vm",
      port.toString)
      .redirectOutput(Redirect.INHERIT)
      .start()
  }

  def vm(vmConfig: VmConfig, blockchainConfig: BlockchainConfig)(implicit actorSystem: ActorSystem): VM =
    (vmConfig.mode, vmConfig.externalConfig) match {
      case (Local, _) => VM
      case (External, Some(ExternalConfig(startVm, host, port))) =>
        startVm match {
          case Mantis =>
            Thread.currentThread().getContextClassLoader match {
              case ucl: URLClassLoader => startMantisVmProcess(ucl.getURLs.map(_.getFile))
              case _ => startMantisVmInThisProcess()
            }
          case Iele => startIeleVmProcess(host, port)
          case None => // nothing
        }
        new ExtVMInterface(host, port, blockchainConfig)
      case _ => throw new RuntimeException("Missing vm.external config for external VM")
    }

}
