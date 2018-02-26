package io.iohk.ethereum.nodebuilder

import java.lang.ProcessBuilder.Redirect

import akka.actor.ActorSystem
import io.iohk.ethereum.extvm.{ExtVMInterface, VmServerApp}
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.utils.{BlockchainConfig, VmConfig}
import io.iohk.ethereum.utils.VmConfig.ExternalConfig


/**
  * Creates the VM and provides ways to interact with and kill additional created processes
  */
class VmSetup(vmConfig: VmConfig, blockchainConfig: BlockchainConfig)(implicit system: ActorSystem) {
  import VmConfig.VmMode._

  val process: Option[Process] = vmConfig.externalConfig.flatMap(startExternalVm)

  val vm = vmConfig.mode match {
    case Internal =>
      new VMImpl

    case External =>
      new ExtVMInterface(vmConfig.externalConfig.get, blockchainConfig)
  }

  def close(): Unit = {
    vm match {
      case extVm: ExtVMInterface => extVm.close()
      case _ =>
    }
    process.foreach(_.destroyForcibly().waitFor())
  }

  private def startExternalVm(externalConfig: ExternalConfig): Option[Process] = {
    externalConfig.vmType match {
      case "iele" | "kevm" =>
        startStandardVmProcess(externalConfig)

      case "mantis" =>
        startMantisVmProcess(externalConfig)

      case "none" =>
        // expect the vm to be started by external means
        None
    }
  }

  /**
    * Runs a standard VM binary that takes $port and $host as input arguments
    */
  private def startStandardVmProcess(externalConfig: ExternalConfig): Option[Process] = {
    import externalConfig._
    require(executablePath.isDefined, s"VM type '$vmType' requires the path to binary to be provided")

    // TODO: we also need host parameter in iele node
    val process = new ProcessBuilder(executablePath.get, port.toString, host)
      .redirectOutput(Redirect.INHERIT)
      .redirectError(Redirect.INHERIT)
      .start()

    Some(process)
  }

  private def startMantisVmProcess(externalConfig: ExternalConfig): Option[Process] = {
    if (externalConfig.executablePath.isDefined)
      startStandardVmProcess(externalConfig)
    else
      startMantisVmInThisProcess()
  }

  private def startMantisVmInThisProcess(): Option[Process] = {
    VmServerApp.main(Array())
    None
  }
}
