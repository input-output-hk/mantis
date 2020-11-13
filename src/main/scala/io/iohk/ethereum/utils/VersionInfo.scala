package io.iohk.ethereum.utils

object VersionInfo {

  /** Produce a node name that get sent in `WireProtocol.Hello.clientId` according to Ethereum conventions.
    *
    * Check out examples on https://etcnodes.org
    *
    * e.g.
    * - mantis/v3.0-cd5ae33/linux-amd64/ubuntu-openjdk64bitservervm-java-11.0.9
    * - besu/v20.10.0/linux-x86_64/oracle_openjdk-java-11
    * - coregeth/v1.11.8-stable-305b5089/linux-amd64/go1.14.4
    */
  val clientId = {
    val appName = BuildInfo.name
    val appVersion = BuildInfo.version
    val appCommit = BuildInfo.gitHeadCommit.map("-" + _.take(7)).getOrElse("")

    val osName = norm(prop("os.name"))
    val osArch = norm(prop("os.arch"))

    val javaVendor = norm(prop("java.vendor"))
    val javaVmName = norm(prop("java.vm.name"))
    val javaVersion = prop("java.version")

    s"$appName/v$appVersion$appCommit/$osName-$osArch/$javaVendor-$javaVmName-java-$javaVersion"
  }

  private def prop(name: String) =
    System.getProperty(name)

  private def norm(value: String) =
    value.toLowerCase.replaceAll("[^a-z0-9]+", "")
}
