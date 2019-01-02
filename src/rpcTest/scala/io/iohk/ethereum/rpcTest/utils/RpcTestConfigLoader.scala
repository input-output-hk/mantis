package io.iohk.ethereum.rpcTest.utils

import io.iohk.ethereum.rpcTest.RpcTestConfig

trait RpcTestConfigLoader {
  implicit val rpcTestConfig = RpcTestConfig("test.conf")
}
