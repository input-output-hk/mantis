package io.iohk.ethereum.faucet

import io.iohk.ethereum.faucet.jsonrpc.FaucetServer
import io.iohk.ethereum.utils.Logger

object Faucet extends Logger {

  def main(args: Array[String]): Unit =
    (new FaucetServer).start()

}
