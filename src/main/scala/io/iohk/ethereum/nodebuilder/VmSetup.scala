package io.iohk.ethereum.nodebuilder

import java.lang.ProcessBuilder.Redirect

import akka.actor.ActorSystem
import io.iohk.ethereum.extvm.{ExtVMInterface, VmServerApp}
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.utils.{BlockchainConfig, VmConfig}
import io.iohk.ethereum.utils.VmConfig.ExternalConfig

object VmSetup {

  import VmConfig.VmMode._

  def vm(vmConfig: VmConfig, blockchainConfig: BlockchainConfig, testMode: Boolean)(implicit actorSystem: ActorSystem): VMImpl =
    (vmConfig.mode, vmConfig.externalConfig) match {
      case (Internal, _) =>
        new VMImpl

      case (External, Some(extConf)) =>
        startExternalVm(extConf)
        new ExtVMInterface(extConf.host, extConf.port, blockchainConfig, testMode)

      case _ =>
        throw new RuntimeException("Missing vm.external config for external VM")
    }

  private def startExternalVm(externalConfig: ExternalConfig): Unit = {
    externalConfig.vmType match {
      case "iele" | "kevm" =>
        startStandardVmProcess(externalConfig)

      case "mantis" =>
        startMantisVmProcess(externalConfig)

      case "none" =>
        // expect the vm to be started by external means
    }
  }

  /**
    * Runs a standard VM binary that takes $port and $host as input arguments
    */
  private def startStandardVmProcess(externalConfig: ExternalConfig): Unit = {
    import externalConfig._
    require(executablePath.isDefined, s"VM type '$vmType' requires the path to binary to be provided")
    // TODO: we also need host parameter in iele node
    new ProcessBuilder(executablePath.get, port.toString, host)
      .redirectOutput(Redirect.INHERIT)
      .redirectError(Redirect.INHERIT)
      .start()
  }

  private def startMantisVmProcess(externalConfig: ExternalConfig): Unit = {
    if (externalConfig.executablePath.isDefined)
      startStandardVmProcess(externalConfig)
    else
      startMantisVmInThisProcess()
  }

  private def startMantisVmInThisProcess(): Unit = {
    VmServerApp.main(Array())
  }

}
