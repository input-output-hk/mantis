package io.iohk.ethereum.nodebuilder

import java.lang.ProcessBuilder.Redirect

import akka.actor.ActorSystem
import io.iohk.ethereum.extvm.{ExtVMInterface, VmServerApp}
import io.iohk.ethereum.utils.{BlockchainConfig, VmConfig}
import io.iohk.ethereum.utils.VmConfig.ExternalConfig
import io.iohk.ethereum.vm.VM

object VmSetup {

  import VmConfig.VmMode._
  import VmConfig.ExternalConfig.StartVmConfig._

  def startStandardVmProcess(executablePath: String, host: String, port: Int): Unit = {
    new ProcessBuilder(executablePath, port.toString, host)
      .redirectOutput(Redirect.INHERIT)
      .start()
  }

  // for running from within SBT
  private def startMantisVmInThisProcess(host: String, port: Int): Unit = {
    VmServerApp.main(Array(host, port.toString))
  }

  def vm(vmConfig: VmConfig, blockchainConfig: BlockchainConfig)(implicit actorSystem: ActorSystem): VM =
    (vmConfig.mode, vmConfig.externalConfig) match {
      case (Internal, _) =>
        VM

      case (External, Some(ExternalConfig(startVmOpt, executablePathOpt, host, port))) =>
        (startVmOpt, executablePathOpt) match {
          case (Some(Mantis), None) => startMantisVmInThisProcess(host, port)
          case (Some(_), Some(executablePath)) => startStandardVmProcess(executablePath, host, port)
          case (Some(_), None) => throw new RuntimeException("Missing `vm.external.executable-path` config")
          case (None, _) => // nothing
        }
        new ExtVMInterface(host, port, blockchainConfig)

      case _ =>
        throw new RuntimeException("Missing vm.external config for external VM")
    }

}
