package io.iohk.ethereum.rpcTest

import com.typesafe.config.ConfigFactory

case class RpcTestConfig(
    mantisUrl: String,
    privateNetDataDir: String,
    keystoreDir: String,
    rpcPatchedBuildDir: String,
    ethminerDir: String)

object RpcTestConfig {

  def apply(confName: String): RpcTestConfig = {
    val config = ConfigFactory.load(confName)

    val mantisUrl = config.getString("mantisUrl")
    val dataDir = config.getString("privatenetDatadir")
    val keystoreDir = config.getString("keystoreDir")
    val rpcPatchedBuildDir = config.getString("rpcPatchedBuildDir")
    val ethminerDir = config.getString("ethminerDir")

    new RpcTestConfig(mantisUrl, dataDir, keystoreDir, rpcPatchedBuildDir, ethminerDir)
  }
}
