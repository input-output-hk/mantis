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
  def nodeName(maybeClientId: Option[String] = None) = {
    val app = {
      val name = BuildInfo.name
      val id = maybeClientId.map("-" + _).getOrElse("")
      s"$name$id"
    }
    val version = {
      val version = BuildInfo.version
      val commit = BuildInfo.gitHeadCommit.map("-" + _.take(7)).getOrElse("")
      s"v$version$commit"
    }
    val os = {
      val name = norm(prop("os.name"))
      val arch = norm(prop("os.arch"))
      s"$name-$arch"
    }
    val vm = {
      val vendor = norm(prop("java.vendor"))
      val vmName = norm(prop("java.vm.name"))
      val version = prop("java.version")
      s"$vendor-$vmName-java-$version"
    }
    s"$app/$version/$os/$vm"
  }

  private def prop(name: String) =
    System.getProperty(name)

  private def norm(value: String) =
    value.toLowerCase.replaceAll("[^a-z0-9]+", "")
}
