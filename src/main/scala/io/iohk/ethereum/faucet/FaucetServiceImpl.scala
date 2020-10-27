package io.iohk.ethereum.faucet

import java.time.Clock

import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.utils.Logger

class FaucetServiceImpl(rpcClient: RpcClient, keyStore: KeyStore, config: FaucetConfig, clock: Clock = Clock.systemUTC())
  extends Logger {

}
