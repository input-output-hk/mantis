package io.iohk.ethereum.nodebuilder

import java.lang.ProcessBuilder.Redirect

import akka.actor.ActorSystem
import io.iohk.ethereum.extvm.ExtVMInterface
import io.iohk.ethereum.utils.{BlockchainConfig, VmConfig}
import io.iohk.ethereum.utils.VmConfig.ExternalConfig
import io.iohk.ethereum.vm.VM

object VmSetup {

  import VmConfig.VmMode._

  def startExternalVm(executablePath: String, host: String, port: Int): Unit = {
    // TODO: we also need host parameter in iele node
    new ProcessBuilder(executablePath, port.toString, host)
      .redirectOutput(Redirect.INHERIT)
      .start()
  }

  def vm(vmConfig: VmConfig, blockchainConfig: BlockchainConfig)(implicit actorSystem: ActorSystem): VM =
    (vmConfig.mode, vmConfig.externalConfig) match {
      case (Internal, _) =>
        VM
      case (External, Some(ExternalConfig(executablePath, host, port))) =>
        startExternalVm(executablePath, host, port)
        new ExtVMInterface(host, port, blockchainConfig)
      case _ =>
        throw new RuntimeException("Missing vm.external config for external VM")
    }

}
