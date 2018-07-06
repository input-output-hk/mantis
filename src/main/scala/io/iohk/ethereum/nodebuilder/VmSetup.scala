package io.iohk.ethereum.nodebuilder

import java.lang.ProcessBuilder.Redirect

import akka.actor.ActorSystem
import io.iohk.ethereum.extvm.{ExtVMInterface, VmServerApp}
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.utils.{BlockchainConfig, Logger, VmConfig}
import io.iohk.ethereum.utils.VmConfig.ExternalConfig

object VmSetup extends Logger {

  import VmConfig.VmMode._

  def vm(vmConfig: VmConfig, blockchainConfig: BlockchainConfig, testMode: Boolean)(implicit actorSystem: ActorSystem): VMImpl =
    (vmConfig.mode, vmConfig.externalConfig) match {
      case (Internal, _) =>
        log.info("Using Mantis internal VM")
        new VMImpl

      case (External, Some(extConf)) =>
        if (extConf.runVm) {
          if (extConf.executablePath.isDefined) startExternalVm(extConf)
          else if (extConf.vmType == ExternalConfig.VmTypeMantis) startMantisVmInThisProcess()
          else throw new RuntimeException("run-vm is set to true, but no executable-path was provided")
        } else {
          log.info("run-vm is set to false. VM process will not be run by Mantis")
        }

        new ExtVMInterface(extConf, blockchainConfig, testMode)

      case _ =>
        throw new RuntimeException("Missing vm.external config for external VM")
    }

  private def startExternalVm(externalConfig: ExternalConfig): Unit = {
    externalConfig.vmType match {
      case ExternalConfig.VmTypeIele | ExternalConfig.VmTypeKevm =>
        log.info(s"Starting external ${externalConfig.vmType} VM process using executable path")
        startStandardVmProcess(externalConfig)

      case ExternalConfig.VmTypeMantis =>
        log.info("Starting external Mantis VM process using executable path")
        startStandardVmProcess(externalConfig)
    }
  }

  /**
    * Runs a standard VM binary that takes $port and $host as input arguments
    */
  private def startStandardVmProcess(externalConfig: ExternalConfig): Unit = {
    import externalConfig._
    new ProcessBuilder(executablePath.get, port.toString, host)
      .redirectOutput(Redirect.INHERIT)
      .redirectError(Redirect.INHERIT)
      .start()
  }

  private def startMantisVmInThisProcess(): Unit = {
    log.info("Starting Mantis VM in the same process.")
    VmServerApp.main(Array())
  }

}
